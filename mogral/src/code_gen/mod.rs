mod error;
mod value;

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::FunctionValue;

use crate::ast;
use crate::op::Op;
use crate::span::Spanned as Sp;
use crate::{MglContext, MglModule};

use self::error::CodeGenError;
use self::value::{MglType, MglValue, MglValueBuilder, MglVariable};

struct CodeGen<'ctx, 'a> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    fpm: Option<PassManager<FunctionValue<'ctx>>>,
    variables: HashMap<String, MglVariable<'ctx>>,
    value_builder: MglValueBuilder<'ctx, 'a>,
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
            variables: HashMap::new(),
            value_builder: MglValueBuilder { context, builder },
        }
    }

    fn gen_module(&mut self, ast: &ast::Module) -> Result<(), Sp<CodeGenError>> {
        for func in &ast.0 {
            match &func.item {
                ast::Func::Extern(e) => self.gen_func_decl(Sp::new(e, func.span)),
                ast::Func::FuncDef(f) => self.gen_func_def(Sp::new(f, func.span)),
            }?;
        }
        Ok(())
    }

    fn gen_func_decl(
        &mut self,
        ast: Sp<&ast::FuncDecl>,
    ) -> Result<FunctionValue<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        // 関数を作成
        let f64_type = self.context.f64_type();
        let param_types: Vec<BasicMetadataTypeEnum> = vec![f64_type.into(); item.params.len()];
        let fn_type = f64_type.fn_type(&param_types, false);
        let fn_val = self.module.add_function(&item.name.item, fn_type, None);

        // 引数の名前を設定
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(&item.params[i].item);
        }

        Ok(fn_val)
    }

    fn gen_func_def(
        &mut self,
        ast: Sp<&ast::FuncDef>,
    ) -> Result<FunctionValue<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        let decl = &item.decl.item;
        let function = self.gen_func_decl(item.decl.as_ref())?;

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        self.variables.clear();

        // 関数の引数を変数表に格納
        self.variables.reserve(decl.params.len());
        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = &decl.params[i];

            let arg = MglValue {
                type_: MglType::Double,
                value: Some(arg),
            };

            let var = self
                .value_builder
                .create_variable(function, MglType::Double, &arg_name.item);
            self.value_builder
                .build_store(var, Sp::new(arg, arg_name.span))?;

            self.variables.insert(decl.params[i].item.clone(), var);
        }

        // 関数本体のコード生成
        let body = self.gen_block(item.body.as_ref())?;

        if let Some(body) = body {
            // return文が無いパスがある場合はここでreturnを作成
            self.value_builder.build_return(body);
        }

        // 関数を検証して最適化を行う
        if function.verify(true) {
            if let Some(fpm) = &self.fpm {
                fpm.run_on(&function);
            }

            Ok(function)
        } else {
            self.module.print_to_stderr();
            unsafe {
                function.delete();
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
            Some(e) => self.gen_expr(e.as_ref()),
            None => Ok(Some(MglValue::unit())),
        }
    }

    fn gen_stmt(&mut self, ast: Sp<&ast::Stmt>) -> Result<Option<()>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        let res = match item {
            ast::Stmt::Assign(a) => self.gen_assign(Sp::new(a, span), false),
            ast::Stmt::Expr(e) => self.gen_expr(Sp::new(e, span)),
        }?;
        // 式の評価結果を捨てる
        Ok(res.and(Some(())))
    }

    /// 変数宣言か再代入のコード生成
    /// (引数のsetがtrueの場合は再代入)
    fn gen_assign(
        &mut self,
        ast: Sp<&ast::Assign>,
        set: bool,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        let var = if set {
            // 再代入の場合、変数が存在しない場合にエラー
            *self.variables.get(&item.var_name.item).ok_or(Sp::new(
                CodeGenError::UnknownVariableName,
                item.var_name.span,
            ))?
        } else {
            // 新規変数宣言の場合、既に変数が存在する場合にエラー
            if self.variables.contains_key(&item.var_name.item) {
                return Err(Sp::new(
                    CodeGenError::VariableAlreadyExists,
                    item.var_name.span,
                ));
            }
            let alloca = self.value_builder.create_variable(
                self.get_func(),
                MglType::Double,
                &item.var_name.item,
            );
            self.variables.insert(item.var_name.item.to_owned(), alloca);
            alloca
        };

        let val = self.gen_expr(item.val.as_ref())?;
        if let Some(val) = val {
            self.value_builder
                .build_store(var, Sp::new(val, item.val.span))?;
        }
        Ok(val)
    }

    fn gen_expr(
        &mut self,
        ast: Sp<&Box<ast::Expr>>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        match item.as_ref() {
            ast::Expr::Set(a) => self.gen_assign(Sp::new(a, span), true),
            ast::Expr::Return(r) => self.gen_return(Sp::new(r, span)),
            ast::Expr::Op(o) => self.gen_op_expr(Sp::new(o, span)),
            ast::Expr::Number(n) => Ok(Some(self.value_builder.double(*n))),
            ast::Expr::Ident(i) => {
                let var = *self
                    .variables
                    .get(i)
                    .ok_or(Sp::new(CodeGenError::UnknownVariableName, span))?;
                Ok(Some(self.value_builder.build_load(var)))
            }
            ast::Expr::FuncCall(fc) => self.gen_func_call(Sp::new(fc, span)),
            ast::Expr::Block(b) => self.gen_block(Sp::new(b, span)),
            ast::Expr::If(i) => self.gen_if(Sp::new(i, span)),
            ast::Expr::For(f) => self.gen_for(Sp::new(f, span)),
        }
    }

    fn gen_return(
        &mut self,
        ast: Sp<&Box<ast::Expr>>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let val = self.gen_expr(ast)?;
        if let Some(val) = val {
            self.value_builder.build_return(val);
        }
        Ok(None)
    }

    fn gen_op_expr(
        &mut self,
        ast: Sp<&ast::OpExpr>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        let lhs = self.gen_expr(item.lhs.as_ref())?;
        let rhs = self.gen_expr(item.rhs.as_ref())?;
        self.value_builder.build_op(item.op.item, lhs, rhs, span)
    }

    fn gen_func_call(
        &mut self,
        ast: Sp<&ast::FuncCall>,
    ) -> Result<Option<MglValue<'ctx>>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        // 関数名から関数を取得
        let func = self
            .module
            .get_function(&item.func_name.item)
            .ok_or(Sp::new(CodeGenError::UnknownFunction, item.func_name.span))?;

        // 引数の数のチェック
        if item.args.len() != func.count_params() as usize {
            return Err(Sp::new(CodeGenError::IncorrectNumberOfArguments, span));
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
            .build_call(func, &args, &item.func_name.item, span)
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

        let parent = self.get_func();

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
        // TODO: thenの値がunitであるか確認する
        if then_val.is_some() {
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

        let parent = self.get_func();

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

        let parent = self.get_func();
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
            .build_op(Op::Lt, Some(index_val), Some(until_val), span)?
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
            .build_op(
                Op::Add,
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

    /// builderが現在処理中の関数を得る
    fn get_func(&self) -> FunctionValue<'ctx> {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
    }
}

pub fn code_gen<'ctx>(
    context: &'ctx MglContext,
    ast: &ast::Module,
    optimize: bool,
) -> Result<MglModule<'ctx>, Box<dyn std::error::Error>> {
    let module = context.0.create_module("mogral");
    let builder = context.0.create_builder();
    let mut code_gen = CodeGen::new(&context.0, &module, &builder, optimize);
    code_gen.gen_module(&ast)?;

    Ok(MglModule(module))
}
