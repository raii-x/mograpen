pub mod error;
mod value;

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::FunctionValue;

use crate::ast;
use crate::pos::Spanned as Sp;
use crate::types::MglType;
use crate::BinOp;

use self::error::CodeGenError;
use self::value::{MglFunction, MglValue, MglValueBuilder, MglVariable};

struct CodeGen<'ctx, 'a> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    fpm: Option<PassManager<FunctionValue<'ctx>>>,
    functions: HashMap<String, MglFunction<'ctx>>,
    variables: HashMap<String, MglVariable<'ctx>>,
    value_builder: MglValueBuilder<'ctx, 'a>,
    current_fn: Option<MglFunction<'ctx>>,
}

impl<'ctx, 'a> CodeGen<'ctx, 'a> {
    fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        optimize: bool,
    ) -> Self {
        let fpm = if optimize {
            // 最適化を行う場合はFPMを作成
            let fpm = PassManager::create(module);

            fpm.add_instruction_combining_pass();
            fpm.add_reassociate_pass();
            fpm.add_gvn_pass();
            fpm.add_cfg_simplification_pass();
            fpm.add_basic_alias_analysis_pass();
            fpm.add_promote_memory_to_register_pass();
            fpm.add_instruction_combining_pass();
            fpm.add_reassociate_pass();

            fpm.initialize();
            Some(fpm)
        } else {
            None
        };

        Self {
            context,
            module,
            builder,
            fpm,
            functions: HashMap::new(),
            variables: HashMap::new(),
            value_builder: MglValueBuilder { context, builder },
            current_fn: None,
        }
    }

    fn gen_module(&mut self, ast: &ast::Module) -> Result<(), Sp<CodeGenError>> {
        for func in &ast.funcs {
            match &func.item {
                ast::Func::Extern(x) => self.gen_func_decl(Sp::new(x, func.span)),
                ast::Func::FuncDef(x) => self.gen_func_def(Sp::new(x, func.span)),
            }?;
        }
        Ok(())
    }

    fn gen_func_decl(
        &mut self,
        ast: Sp<&ast::FuncDecl>,
    ) -> Result<MglFunction<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        let fn_name = &item.name.item;

        // 同じ名前の関数が既にあればエラー
        if self.functions.contains_key(fn_name) {
            return Err(Sp::new(
                CodeGenError::MultipleDefinitions(fn_name.clone()),
                span,
            ));
        }

        // 引数型を作成
        let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::with_capacity(item.params.len());
        let mut param_names: Vec<&str> = Vec::with_capacity(item.params.len());

        for param in &item.params {
            let Sp {
                item: p_item,
                span: _,
            } = param;

            // MglTypeに対応するinkwellの型がある引数のみを追加
            if let Some(ty) = self.value_builder.ink_type(p_item.type_.item) {
                param_types.push(ty.into());
                param_names.push(&p_item.ident.item);
            }
        }

        // 関数型を作成
        let fn_type = match item.ret {
            Some(Sp { item: ty, span: _ }) => match self.value_builder.ink_type(ty) {
                Some(ty) => ty.fn_type(&param_types, false),
                None => self.context.void_type().fn_type(&param_types, false),
            },
            None => self.context.void_type().fn_type(&param_types, false),
        };

        // 関数を作成
        let fn_val = self.module.add_function(&fn_name, fn_type, None);

        // 引数の名前を設定
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.set_name(param_names[i]);
        }

        // MglFunctionを作成
        let func = MglFunction {
            params: item.params.iter().map(|x| x.item.type_.item).collect(),
            ret_type: match &item.ret {
                Some(ty) => ty.item,
                None => MglType::Unit,
            },
            value: fn_val,
        };

        // 関数を関数表に格納
        self.functions.insert(fn_name.clone(), func.clone());

        Ok(func)
    }

    fn gen_func_def(
        &mut self,
        ast: Sp<&ast::FuncDef>,
    ) -> Result<MglFunction<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        // 関数を作成
        let func = self.gen_func_decl(item.decl.as_ref())?;

        self.current_fn = Some(func.clone());

        let ret = self.gen_func_def_inner(ast, func);

        self.current_fn = None;

        ret
    }

    fn gen_func_def_inner(
        &mut self,
        ast: Sp<&ast::FuncDef>,
        func: MglFunction<'ctx>,
    ) -> Result<MglFunction<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        // 基本ブロックを作成
        let entry = self.context.append_basic_block(func.value, "entry");
        self.builder.position_at_end(entry);

        self.variables.clear();

        // 関数の引数を変数表に格納
        self.variables.reserve(func.params.len());
        let mut param_iter = func.value.get_param_iter();
        for (i, &type_) in func.params.iter().enumerate() {
            let arg_name = &item.decl.item.params[i].item.ident;

            // 引数の変数を作成
            let var = self
                .value_builder
                .create_variable(func.value, type_, &arg_name.item);

            // 変数表に変数を格納
            self.variables.insert(arg_name.item.clone(), var);

            // 引数値を取得
            let arg_value = MglValue {
                type_,
                // MglTypeに対応するinkwell型がある場合は、inkwellの引数の値を格納
                value: self
                    .value_builder
                    .ink_type(type_)
                    .and_then(|_| Some(param_iter.next().unwrap())),
            };

            // 引数値を変数に格納
            self.value_builder
                .build_store(var, Sp::new(arg_value, arg_name.span))?;
        }

        // 関数本体のコード生成
        let body = self.gen_block(item.body.as_ref())?;

        if let Some(body) = body {
            // return文が無いパスがある場合はここでreturnを作成
            self.value_builder.build_return(
                self.current_fn.as_ref().unwrap().ret_type,
                Sp::new(body, item.body.span),
            )?;
        }

        // 関数を検証して最適化を行う
        if func.value.verify(true) {
            if let Some(fpm) = &self.fpm {
                fpm.run_on(&func.value);
            }

            Ok(func)
        } else {
            self.module.print_to_stderr();
            unsafe {
                func.value.delete();
            }

            Err(Sp::new(CodeGenError::InvalidFunction, span))
        }
    }

    fn gen_block(
        &mut self,
        ast: Sp<&ast::Block>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        // 文のコード生成
        for s in &item.stmts {
            let res = self.gen_stmt(s.as_ref())?;
            if res.is_none() {
                // returnするならそれ以降のコード生成を行わない
                return Ok(None);
            }
        }

        // 式があれば生成して評価結果を返し、式が無ければunitを返す
        match &item.expr {
            Some(x) => self.gen_expr(x.as_ref()),
            None => Ok(Some(MglValue::unit())),
        }
    }

    fn gen_stmt(&mut self, ast: Sp<&ast::Stmt>) -> Result<Option<()>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        let res = match item {
            ast::Stmt::Let(x) => self.gen_let(x.as_ref()),
            ast::Stmt::Expr(x) => self.gen_expr(x.as_ref()),
        }?;
        // 式の評価結果を捨てる
        Ok(res.and(Some(())))
    }

    fn gen_let(&mut self, ast: Sp<&ast::Let>) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;
        let var_name = &item.var.item.ident;

        // 既に変数が存在する場合にエラー
        if self.variables.contains_key(&var_name.item) {
            return Err(Sp::new(
                CodeGenError::VariableAlreadyExists(var_name.item.clone()),
                var_name.span,
            ));
        }

        // 初期値のコード生成
        let val = self.gen_expr(item.val.as_ref())?;

        // returnするならそれ以降のコード生成を行わない
        let val = match val {
            Some(x) => x,
            None => return Ok(None),
        };

        // 型注釈または初期値から型を取得
        let ty = match &item.var.item.type_ {
            Some(ty) => {
                // 型注釈が初期値の型と合っていない場合にエラー
                if ty.item != val.type_ {
                    return Err(Sp::new(
                        CodeGenError::MismatchedTypes {
                            expected: ty.item,
                            found: val.type_,
                        },
                        item.val.span,
                    ));
                }
                ty.item
            }
            None => {
                // 初期値の型を使用
                val.type_
            }
        };

        // 変数を作成
        let var = self.value_builder.create_variable(
            self.current_fn.as_ref().unwrap().value,
            ty,
            &var_name.item,
        );
        self.variables.insert(var_name.item.clone(), var);

        // 初期値を代入
        self.value_builder
            .build_store(var, Sp::new(val, item.val.span))?;
        Ok(Some(val))
    }

    fn gen_expr(
        &mut self,
        ast: Sp<&Box<ast::Expr>>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        match item.as_ref() {
            ast::Expr::Set(x) => self.gen_set(Sp::new(x, span)),
            ast::Expr::Return(x) => self.gen_return(Sp::new(x, span)),
            ast::Expr::BinOp(x) => self.gen_bin_op_expr(Sp::new(x, span)),
            ast::Expr::LazyBinOp(x) => self.gen_lazy_bin_op_expr(Sp::new(x, span)),
            ast::Expr::UnOp(x) => self.gen_un_op_expr(Sp::new(x, span)),
            ast::Expr::Literal(x) => self.gen_literal(Sp::new(x, span)),
            ast::Expr::Ident(s) => {
                let var = *self
                    .variables
                    .get(s)
                    .ok_or(Sp::new(CodeGenError::UnresolvedName(s.to_owned()), span))?;
                Ok(Some(self.value_builder.build_load(var)))
            }
            ast::Expr::FuncCall(x) => self.gen_func_call(Sp::new(x, span)),
            ast::Expr::Block(x) => self.gen_block(Sp::new(x, span)),
            ast::Expr::If(x) => self.gen_if(Sp::new(x, span)),
            ast::Expr::For(x) => self.gen_for(Sp::new(x, span)),
        }
    }

    fn gen_set(&mut self, ast: Sp<&ast::Set>) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        // 変数を取得
        let var = *self.variables.get(&item.var_name.item).ok_or(Sp::new(
            // 変数が存在しない場合にエラー
            CodeGenError::UnresolvedName(item.var_name.item.to_owned()),
            item.var_name.span,
        ))?;

        // 値を代入
        let val = self.gen_expr(item.val.as_ref())?;
        if let Some(val) = val {
            self.value_builder
                .build_store(var, Sp::new(val, item.val.span))?;
        }
        Ok(val)
    }

    fn gen_return(
        &mut self,
        ast: Sp<&Box<ast::Expr>>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let span = ast.span;
        let val = self.gen_expr(ast)?;
        if let Some(val) = val {
            self.value_builder.build_return(
                self.current_fn.as_ref().unwrap().ret_type,
                Sp::new(val, span),
            )?;
        }
        Ok(None)
    }

    fn gen_bin_op_expr(
        &mut self,
        ast: Sp<&ast::BinOpExpr>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        // TODO: 左辺がreturnする場合に右辺を評価しないようにする
        let lhs = self.gen_expr(item.lhs.as_ref())?;
        let rhs = self.gen_expr(item.rhs.as_ref())?;
        self.value_builder
            .build_bin_op(item.op.item, lhs, rhs, span)
    }

    fn gen_lazy_bin_op_expr(
        &mut self,
        ast: Sp<&ast::LazyBinOpExpr>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        let func = self.current_fn.as_ref().unwrap().value;

        // 左辺のコード生成
        let lhs = self.gen_expr(item.lhs.as_ref())?;
        let lhs_bb = self.builder.get_insert_block().unwrap();

        // 右辺用の基本ブロックを作成
        let rhs_bb_begin = self.context.append_basic_block(func, "lazy_bin_rhs");
        self.builder.position_at_end(rhs_bb_begin);

        // 右辺のコード生成
        let rhs = self.gen_expr(item.rhs.as_ref())?;
        let rhs_bb_end = self.builder.get_insert_block().unwrap();

        // 演算子のコード生成
        self.value_builder.build_lazy_bin_op(
            func,
            item.op.item,
            Sp::new(lhs, ast.item.lhs.span),
            lhs_bb,
            Sp::new(rhs, ast.item.rhs.span),
            rhs_bb_begin,
            rhs_bb_end,
        )
    }

    fn gen_un_op_expr(
        &mut self,
        ast: Sp<&ast::UnOpExpr>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        let opnd = self.gen_expr(item.opnd.as_ref())?;
        self.value_builder.build_un_op(item.op.item, opnd, span)
    }

    fn gen_literal(
        &mut self,
        ast: Sp<&ast::Literal>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        match item {
            ast::Literal::Unit(_) => Ok(Some(MglValue::unit())),
            ast::Literal::Float(v) => Ok(Some(self.value_builder.double(*v))),
            ast::Literal::Bool(v) => Ok(Some(self.value_builder.bool(*v))),
        }
    }

    fn gen_func_call(
        &mut self,
        ast: Sp<&ast::FuncCall>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        // 関数名から関数を取得
        let func = self
            .functions
            .get(&item.func_name.item)
            .ok_or(Sp::new(
                CodeGenError::UnresolvedName(item.func_name.item.to_owned()),
                item.func_name.span,
            ))?
            .clone();

        // 引数の数のチェック
        if item.args.len() != func.params.len() {
            return Err(Sp::new(
                CodeGenError::InvalidNumberOfArguments {
                    expected: func.params.len(),
                    found: item.args.len(),
                },
                span,
            ));
        }

        // 引数の配列を作成
        let mut args = Vec::with_capacity(item.args.len());

        for arg in &item.args {
            match self.gen_expr(arg.as_ref())? {
                Some(v) => args.push(Sp::new(v, arg.span)),
                None => return Ok(None),
            }
        }

        // 関数呼び出し命令を作成
        self.value_builder
            .build_call(&func, &args, &item.func_name.item)
    }

    fn gen_if(&mut self, ast: Sp<&ast::If>) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        // 条件式の処理
        let cond = self.gen_expr(item.cond.as_ref())?;
        let cond = match cond {
            Some(v) => {
                // 型チェック
                if v.type_ != MglType::Bool {
                    return Err(Sp::new(
                        CodeGenError::MismatchedTypes {
                            expected: MglType::Bool,
                            found: v.type_,
                        },
                        item.cond.span,
                    ));
                }
                v
            }
            // returnするならそれ以降のコード生成を行わない
            None => return Ok(None),
        };

        match &item.else_ {
            Some(_) => self.gen_if_else(ast, cond),
            None => self.gen_if_without_else(ast, cond),
        }
    }

    fn gen_if_without_else(
        &mut self,
        ast: Sp<&ast::If>,
        cond: MglValue,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        let parent = self.current_fn.as_ref().unwrap().value;

        // 分岐用の基本ブロックを作成
        let then_bb = self.context.append_basic_block(parent, "then");
        let merge_bb = self.context.append_basic_block(parent, "ifcont");

        self.builder.build_conditional_branch(
            cond.value.unwrap().into_int_value(),
            then_bb,
            merge_bb,
        );

        // ======================================== then_bbの処理
        self.builder.position_at_end(then_bb);

        // thenのコード生成
        let then_val = self.gen_block(item.then.as_ref())?;

        if let Some(then_val) = then_val {
            // thenの値がunitでない場合はエラー
            if then_val.type_ != MglType::Unit {
                return Err(Sp::new(
                    CodeGenError::MismatchedTypes {
                        expected: MglType::Unit,
                        found: then_val.type_,
                    },
                    ast.item.then.span,
                ));
            }
            self.builder.build_unconditional_branch(merge_bb);
        }

        // ======================================== merge_bbの処理
        self.builder.position_at_end(merge_bb);

        // elseの無いif式は常にunitを返す
        Ok(Some(MglValue::unit()))
    }

    fn gen_if_else(
        &mut self,
        ast: Sp<&ast::If>,
        cond: MglValue,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        let parent = self.current_fn.as_ref().unwrap().value;

        // if式を評価した値を格納する変数
        let mut if_var = None;

        // 分岐用の基本ブロックを作成
        let then_bb = self.context.append_basic_block(parent, "then");
        let else_bb = self.context.append_basic_block(parent, "else");
        let merge_bb = self.context.append_basic_block(parent, "ifcont");

        self.builder.build_conditional_branch(
            cond.value.unwrap().into_int_value(),
            then_bb,
            else_bb,
        );

        // ======================================== then_bbの処理
        self.builder.position_at_end(then_bb);

        // thenのコード生成
        let then_val = self.gen_block(item.then.as_ref())?;

        if let Some(then_val) = then_val {
            // 変数を作成して格納
            if_var = Some(
                self.value_builder
                    .create_variable(parent, then_val.type_, "ifvalue"),
            );
            self.value_builder
                .build_store(if_var.unwrap(), Sp::new(then_val, item.then.span))?;

            self.builder.build_unconditional_branch(merge_bb);
        }

        // ======================================== else_bbの処理
        self.builder.position_at_end(else_bb);

        let else_ = item.else_.as_ref().unwrap();

        // elseのコード生成
        let else_val = self.gen_block(else_.as_ref())?;

        if let Some(else_val) = else_val {
            match if_var {
                Some(var) => {
                    if else_val.type_ != var.type_ {
                        // thenとelseの型が合っていなければエラー
                        return Err(Sp::new(
                            CodeGenError::MismatchedTypes {
                                expected: var.type_,
                                found: else_val.type_,
                            },
                            else_.span,
                        ));
                    }
                }
                None => {
                    // 変数を作成
                    if_var = Some(self.value_builder.create_variable(
                        parent,
                        else_val.type_,
                        "ifvalue",
                    ));
                }
            }
            // 変数に格納
            self.value_builder
                .build_store(if_var.unwrap(), Sp::new(else_val, else_.span))?;

            self.builder.build_unconditional_branch(merge_bb);
        }

        // ======================================== merge_bbの処理

        match if_var {
            Some(var) => {
                // merge_bbを現在の位置に挿入
                self.builder.position_at_end(merge_bb);
                Ok(Some(self.value_builder.build_load(var)))
            }
            None => {
                // thenとelseの両方の場合でreturnする場合はそれ以降のコード生成を行わない
                merge_bb.remove_from_function().unwrap();
                Ok(None)
            }
        }
    }

    fn gen_for(&mut self, ast: Sp<&ast::For>) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        let parent = self.current_fn.as_ref().unwrap().value;
        let var_name = &item.var_name.item;

        // インデックス変数を作成
        let index_var = self
            .value_builder
            .create_variable(parent, MglType::Double, var_name);
        self.value_builder.build_store(
            index_var,
            Sp::new(self.value_builder.double(0.0), item.var_name.span),
        )?;

        // 同名の変数があればシャドーイングした後、変数を登録
        let old_val = self.variables.remove(var_name);
        self.variables.insert(var_name.to_owned(), index_var);

        // 条件判定部分の基本ブロック
        let head_bb = self.context.append_basic_block(parent, "loop_head");

        self.builder.build_unconditional_branch(head_bb);

        // ======================================== head_bbの処理
        self.builder.position_at_end(head_bb);

        let until_val = self.gen_expr(item.until.as_ref())?;
        let until_val = match until_val {
            Some(v) => v,
            None => return Ok(None),
        };

        // インデックスを取得
        let index_val = self.value_builder.build_load(index_var);

        let end_cond = self
            .value_builder
            .build_bin_op(BinOp::Lt, Some(index_val), Some(until_val), span)?
            .unwrap();

        let body_bb = self.context.append_basic_block(parent, "loop_body");
        let after_bb = self.context.append_basic_block(parent, "after_loop");

        self.builder.build_conditional_branch(
            end_cond.value.unwrap().into_int_value(),
            body_bb,
            after_bb,
        );

        // ======================================== body_bbの処理
        self.builder.position_at_end(body_bb);

        // 本体のコードを生成
        if self.gen_block(item.body.as_ref())?.is_none() {
            return Ok(None);
        }

        // インデックスを取得
        let index_val = self.value_builder.build_load(index_var);

        // インクリメントしてストア
        let next_index_val = self
            .value_builder
            .build_bin_op(
                BinOp::Add,
                Some(index_val),
                Some(self.value_builder.double(1.0)),
                span,
            )?
            .unwrap();
        self.value_builder
            .build_store(index_var, Sp::new(next_index_val, item.var_name.span))?;

        self.builder.build_unconditional_branch(head_bb);

        // ======================================== after_bbの処理
        self.builder.position_at_end(after_bb);

        // 変数の登録を削除
        self.variables.remove(var_name);

        // シャドーイングした変数があれば戻す
        if let Some(val) = old_val {
            self.variables.insert(var_name.to_owned(), val);
        }

        Ok(Some(MglValue::unit()))
    }
}

pub fn code_gen<'ctx>(
    context: &'ctx Context,
    ast: &ast::Module,
    optimize: bool,
) -> Result<Module<'ctx>, Sp<CodeGenError>> {
    let module = context.create_module("mogral");
    let builder = context.create_builder();
    let mut code_gen = CodeGen::new(&context, &module, &builder, optimize);
    code_gen.gen_module(&ast)?;

    Ok(module)
}
