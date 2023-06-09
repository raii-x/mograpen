use std::error::Error;

use egui::{Key, KeyboardShortcut, Modifiers};
use mogral::{
    ast::{self, ASTNode},
    code_gen,
    inkwell::{context::Context, module::Module, OptimizationLevel},
    parse, parse_error_pos, SourcePosConverter,
};
use strum_macros::{Display, EnumString, IntoStaticStr};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Display,
    EnumString,
    IntoStaticStr,
    serde::Deserialize,
    serde::Serialize,
)]
enum Tab {
    #[strum(serialize = "Editor")]
    Editor,
    #[strum(serialize = "Error")]
    Error,
    #[strum(serialize = "AST")]
    Ast,
    #[strum(serialize = "LLVM IR")]
    LlvmIr,
    #[strum(serialize = "Exec Result")]
    ExecResult,
}

/// We derive Deserialize/Serialize so we can persist app state on shutdown.
#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)] // if we add new fields, give them default values when deserializing old state
pub struct Editor {
    tree: egui_dock::Tree<Tab>,
    context: AppContext,

    #[serde(skip)]
    pub style: egui_dock::Style,
}

impl Default for Editor {
    fn default() -> Self {
        let mut style = egui_dock::Style::from_egui(&egui::Style::default());
        style.separator.width = 2.; // 1ピクセルだとセパレータをドラッグできなかったので2ピクセルにする

        Self {
            // Example stuff:
            tree: Self::default_layout(),
            context: AppContext::new(),
            style,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExecuteStage {
    Parse,
    CodeGen,
    Execute,
}

impl Editor {
    /// Called once before the first frame.
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        // This is also where you can customize the look and feel of egui using
        // `cc.egui_ctx.set_visuals` and `cc.egui_ctx.set_fonts`.

        // Load previous app state (if any).
        // Note that you must enable the `persistence` feature for this to work.
        if let Some(storage) = cc.storage {
            return eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
        }

        Default::default()
    }

    fn default_layout() -> egui_dock::Tree<Tab> {
        let mut tree = egui_dock::Tree::new(vec![Tab::Editor]);
        let node_editor_ast = tree.split_right(egui_dock::NodeIndex::root(), 0.4, vec![Tab::Ast]);
        let node_ast_llvm_ir = tree.split_right(node_editor_ast[1], 0.4, vec![Tab::LlvmIr]);
        tree.split_below(node_editor_ast[0], 0.75, vec![Tab::Error]);
        tree.split_below(node_ast_llvm_ir[1], 0.75, vec![Tab::ExecResult]);
        tree
    }
}

impl eframe::App for Editor {
    /// Called by the frame work to save state before shutdown.
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    /// Called each time the UI needs repainting, which may be many times per second.
    /// Put your widgets into a `SidePanel`, `TopPanel`, `CentralPanel`, `Window` or `Area`.
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        ctx.input_mut(|i| {
            // F5キーで実行
            if i.consume_shortcut(&KeyboardShortcut {
                modifiers: Modifiers::NONE,
                key: Key::F5,
            }) {
                self.context.execute(ExecuteStage::Execute);
            }
        });

        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            // The top panel is often a good place for a menu bar:
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Save IIVM IR").clicked() {
                        std::fs::write("code.ll", &self.context.llvm_ir).unwrap();
                    }
                    if ui.button("Reset layout").clicked() {
                        self.tree = Self::default_layout();
                    }
                    if ui.button("Quit").clicked() {
                        frame.close();
                    }
                });
                ui.menu_button("Execute", |ui| {
                    if ui.button("Execute file").clicked() {
                        self.context.execute(ExecuteStage::Execute);
                    }
                    ui.checkbox(&mut self.context.exec_on_edit, "Execute on edit");
                    ui.checkbox(&mut self.context.optimize, "Optimize");
                    if ui.button("Until parse").clicked() {
                        self.context.execute(ExecuteStage::Parse);
                    }
                    if ui.button("Until code generation").clicked() {
                        self.context.execute(ExecuteStage::CodeGen);
                    }
                });
            });
        });

        egui_dock::DockArea::new(&mut self.tree)
            .style(self.style.clone())
            .show_close_buttons(false)
            .show(ctx, &mut self.context);
        // The central panel the region left after adding TopPanel's and SidePanel's
    }
}

#[derive(serde::Deserialize, serde::Serialize)]
struct AppContext {
    code: String,
    #[serde(skip)]
    pub error: String,
    #[serde(skip)]
    pub ast: Option<ast::Module>,
    #[serde(skip)]
    pub llvm_ir: String,
    #[serde(skip)]
    pub exec_result: String,

    optimize: bool,
    exec_on_edit: bool,

    #[serde(skip)]
    cursor_pos: Option<(usize, usize)>,
}

impl AppContext {
    fn new() -> Self {
        Self {
            code: String::new(),
            error: String::new(),
            ast: None,
            llvm_ir: String::new(),
            exec_result: String::new(),
            optimize: true,
            exec_on_edit: false,
            cursor_pos: None,
        }
    }

    fn execute(&mut self, until_stage: ExecuteStage) {
        self.error.clear();
        self.ast = None;
        self.llvm_ir.clear();
        self.exec_result.clear();

        let conv = SourcePosConverter::new(&self.code);

        // パース

        self.ast = Some(match parse(&self.code) {
            Ok(ast) => ast,
            Err(e) => {
                let (line, col) = conv.pos_to_line_col(parse_error_pos(&e));

                self.error = format!("{}:{}: {}", line, col, e);
                return;
            }
        });
        let ast = self.ast.as_ref().unwrap();

        if until_stage == ExecuteStage::Parse {
            return;
        }

        // コード生成

        let context = Context::create();
        let module = match code_gen(&context, ast, self.optimize) {
            Ok(module) => module,
            Err(e) => {
                let (line, col) = conv.pos_to_line_col(e.span.l);
                self.error = format!("{}:{}: {}", line, col, e.item.to_string());
                return;
            }
        };
        self.llvm_ir = module.to_string();

        if until_stage == ExecuteStage::CodeGen {
            return;
        }

        // 実行

        let exec_result = match self.execute_inner(&module) {
            Ok(result) => result,
            Err(e) => {
                self.error = e.to_string();
                return;
            }
        };
        self.exec_result = exec_result.node_to_string();
    }

    fn execute_inner(&mut self, module: &Module<'_>) -> Result<f64, Box<dyn Error>> {
        let exec_engine = module.create_jit_execution_engine(OptimizationLevel::Default)?;

        unsafe {
            let func = exec_engine.get_function::<unsafe extern "C" fn(f64) -> f64>("main")?;
            Ok(func.call(1.0))
        }
    }
}

impl egui_dock::TabViewer for AppContext {
    type Tab = Tab;

    fn ui(&mut self, ui: &mut egui::Ui, tab: &mut Tab) {
        match *tab {
            Tab::Editor => {
                match self.cursor_pos {
                    Some(pos) => ui.label(format!("Ln {}, Col {}", pos.0 + 1, pos.1 + 1)),
                    None => ui.label(""),
                };

                egui::ScrollArea::vertical().show(ui, |ui| {
                    let editor = egui::TextEdit::multiline(&mut self.code)
                        .code_editor()
                        .desired_width(f32::INFINITY)
                        .show(ui);

                    // TODO: 長い行が折り返された場合に折り返された後の位置が表示されるのを修正する
                    self.cursor_pos = editor.cursor_range.map(|pos| {
                        let c = pos.primary.rcursor;
                        (c.row, c.column)
                    });

                    if editor.response.changed() && self.exec_on_edit {
                        self.execute(ExecuteStage::Execute);
                    }
                });
            }
            Tab::Error => {
                ui.label(&self.error);
            }
            Tab::Ast => {
                if let Some(ast) = &self.ast {
                    show_ast(ui, ast);
                }
            }
            Tab::LlvmIr => {
                ui.label(&self.llvm_ir);
            }
            Tab::ExecResult => {
                ui.label(&self.exec_result);
            }
        }
    }

    fn title(&mut self, tab: &mut Tab) -> egui::WidgetText {
        let s: &str = (*tab).into();
        s.into()
    }
}

fn show_ast_inner(ui: &mut egui::Ui, name: String, ast: &dyn ASTNode) {
    if ast.children().is_empty() {
        ui.label("      ".to_owned() + &name + ": " + &ast.node_to_string());
    } else {
        egui::CollapsingHeader::new(name + ": " + &ast.node_to_string())
            .default_open(true)
            .show(ui, |ui| {
                for child in ast.children() {
                    show_ast_inner(ui, child.0, child.1);
                }
            });
    }
}

fn show_ast(ui: &mut egui::Ui, ast: &dyn ASTNode) {
    egui::CollapsingHeader::new(ast.node_to_string())
        .default_open(true)
        .show(ui, |ui| {
            for child in ast.children() {
                show_ast_inner(ui, child.0, child.1);
            }
        });
}
