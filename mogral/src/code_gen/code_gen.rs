use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::FunctionValue;

use crate::pos::Spanned as Sp;
use crate::types::MglType;
use crate::BinOp;
use crate::{ast, unwrap_never};

use super::builder::MglBuilder;
use super::error::CodeGenError;
use super::value::{MaybeNever, MglFunction, PlaceExpr, ValueExpr};

struct CodeGen<'ctx, 'a> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    fpm: Option<PassManager<FunctionValue<'ctx>>>,
    mgl_builder: &'a MglBuilder<'ctx, 'a>,
    functions: HashMap<String, MglFunction<'ctx>>,
    variables: HashMap<String, PlaceExpr<'ctx>>,
    current_fn: Option<MglFunction<'ctx>>,
}

impl<'ctx, 'a> CodeGen<'ctx, 'a> {
    fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        optimize: bool,
        mgl_builder: &'a MglBuilder<'ctx, 'a>,
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
            mgl_builder,
            functions: HashMap::new(),
            variables: HashMap::new(),
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
            if let Some(ty) = self.mgl_builder.ink_type(&p_item.type_.item) {
                param_types.push(ty.into());
                param_names.push(&p_item.ident.item);
            }
        }

        // 関数型を作成
        let fn_type = match &item.ret {
            Some(Sp { item: ty, span: _ }) => match self.mgl_builder.ink_type(&ty) {
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
            params: item
                .params
                .iter()
                .map(|x| x.item.type_.item.clone())
                .collect(),
            ret_type: match &item.ret {
                Some(ty) => ty.item.clone(),
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

        // 引数の値を生成
        let args = self.mgl_builder.build_args(&func);

        // 関数の引数を変数表に格納
        self.variables.reserve(func.params.len());

        for (i, type_) in func.params.iter().enumerate() {
            let arg_name = &item.decl.item.params[i].item.ident;

            // 引数の変数を作成
            let var = self
                .mgl_builder
                .create_variable(func.value, type_, &arg_name.item);

            // 変数表に変数を格納
            self.variables.insert(arg_name.item.clone(), var.clone());

            // 引数値を変数に格納
            self.mgl_builder
                .build_store(&var, Sp::new(&args[i].clone().into(), arg_name.span))?;
        }

        // 関数本体のコード生成
        let body = self.gen_block(item.body.as_ref())?;

        if let MaybeNever::Value(body) = body {
            // return文が無いパスがある場合はここでreturnを作成
            let body = self.mgl_builder.build_expr(body);

            self.mgl_builder.build_return(
                &self.current_fn.as_ref().unwrap().ret_type,
                Sp::new(&body, item.body.span),
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

    fn gen_block(&mut self, ast: Sp<&ast::Block>) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        // 文のコード生成
        for s in &item.stmts {
            // returnするならそれ以降のコード生成を行わない
            unwrap_never!(self.gen_stmt(s.as_ref())?);
        }

        // 式があれば生成して評価結果を返し、式が無ければunitを返す
        match &item.expr {
            Some(x) => self.gen_expr(x.as_ref()),
            None => Ok(ValueExpr::unit().into()),
        }
    }

    fn gen_stmt(&mut self, ast: Sp<&ast::Stmt>) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        unwrap_never!(match item {
            ast::Stmt::Let(x) => self.gen_let(x.as_ref()),
            ast::Stmt::Expr(x) => self.gen_expr(x.as_ref()),
        }?);

        Ok(ValueExpr::unit().into())
    }

    fn gen_let(&mut self, ast: Sp<&ast::Let>) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        // 既に変数が存在する場合にエラー
        if self.variables.contains_key(&item.name.item) {
            return Err(Sp::new(
                CodeGenError::VariableAlreadyExists(item.name.item.clone()),
                item.name.span,
            ));
        }

        // 初期値
        let init_val = match &item.val {
            // 初期値がある場合
            Some(val) => {
                // 初期値のコード生成
                // returnするならそれ以降のコード生成を行わない
                let expr = unwrap_never!(self.gen_expr(val.as_ref())?);
                Some(self.mgl_builder.build_expr(expr))
            }
            // 初期値がなければNone
            None => None,
        };

        // 型注釈または初期値から型を取得
        let ty = match &item.type_ {
            Some(ty) => {
                // 型注釈が初期値の型と合っていない場合にエラー
                if let Some(val) = &init_val {
                    if ty.item != val.type_ {
                        return Err(Sp::new(
                            CodeGenError::MismatchedTypes {
                                expected: ty.item.clone(),
                                found: val.type_.clone(),
                            },
                            item.val.as_ref().unwrap().span,
                        ));
                    }
                }
                // 型注釈の型を使用
                &ty.item
            }
            None => {
                match &init_val {
                    // 初期値の型を使用
                    Some(val) => &val.type_,
                    // 初期値も型注釈もない変数宣言は文法には無い
                    None => unreachable!(),
                }
            }
        };

        // 変数を作成
        let var = self.mgl_builder.create_variable(
            self.current_fn.as_ref().unwrap().value,
            ty,
            &item.name.item,
        );
        self.variables.insert(item.name.item.clone(), var.clone());

        // 初期値を代入
        if let Some(val) = &init_val {
            self.mgl_builder
                .build_store(&var, Sp::new(val, item.val.as_ref().unwrap().span))?;
        }

        Ok(ValueExpr::unit().into())
    }

    fn gen_expr(&mut self, ast: Sp<&Box<ast::Expr>>) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        match item.as_ref() {
            ast::Expr::Set(x) => self.gen_set(Sp::new(x, span)),
            ast::Expr::Return(x) => self.gen_return(Sp::new(x, span)),
            ast::Expr::BinOp(x) => self.gen_bin_op_expr(Sp::new(x, span)),
            ast::Expr::LazyBinOp(x) => self.gen_lazy_bin_op_expr(Sp::new(x, span)),
            ast::Expr::UnOp(x) => self.gen_un_op_expr(Sp::new(x, span)),
            ast::Expr::Index(x) => self.gen_index(Sp::new(x, span)),
            ast::Expr::Literal(x) => self.gen_literal(Sp::new(x, span)).map(|x| x.into()),
            ast::Expr::Ident(s) => self.gen_ident(Sp::new(s, span)),
            ast::Expr::FuncCall(x) => self.gen_func_call(Sp::new(x, span)),
            ast::Expr::Block(x) => self.gen_block(Sp::new(x, span)),
            ast::Expr::If(x) => self.gen_if(Sp::new(x, span)),
            ast::Expr::For(x) => self.gen_for(Sp::new(x, span)),
        }
    }

    fn gen_set(&mut self, ast: Sp<&ast::Set>) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        // 変数を取得
        let var = unwrap_never!(self.gen_expr(item.var.as_ref())?);
        let var = var
            .try_into()
            .map_err(|_| Sp::new(CodeGenError::AssignmentToNonPlaceExpression, item.var.span))?;

        let val = unwrap_never!(self.gen_expr(item.val.as_ref())?);
        let val = self.mgl_builder.build_expr(val);

        // 値を代入
        self.mgl_builder
            .build_store(&var, Sp::new(&val, item.val.span))?;
        Ok(val.into())
    }

    fn gen_return(
        &mut self,
        ast: Sp<&Box<ast::Expr>>,
    ) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let span = ast.span;

        let val = unwrap_never!(self.gen_expr(ast)?);
        let val = self.mgl_builder.build_expr(val);

        self.mgl_builder.build_return(
            &self.current_fn.as_ref().unwrap().ret_type,
            Sp::new(&val, span),
        )?;

        Ok(MaybeNever::Never)
    }

    fn gen_bin_op_expr(
        &mut self,
        ast: Sp<&ast::BinOpExpr>,
    ) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        // 左辺でreturnする場合は右辺を評価しない
        let lhs = unwrap_never!(self.gen_expr(item.lhs.as_ref())?);
        let lhs = self.mgl_builder.build_expr(lhs);

        // 右辺でreturnする場合は演算を行わない
        let rhs = unwrap_never!(self.gen_expr(item.rhs.as_ref())?);
        let rhs = self.mgl_builder.build_expr(rhs);

        self.mgl_builder
            .build_bin_op(item.op.item, &lhs, &rhs, span)
            .map(|x| x.into())
    }

    fn gen_lazy_bin_op_expr(
        &mut self,
        ast: Sp<&ast::LazyBinOpExpr>,
    ) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        let func = self.current_fn.as_ref().unwrap().value;

        // 左辺のコード生成
        let lhs = unwrap_never!(self.gen_expr(item.lhs.as_ref())?);
        let lhs = self.mgl_builder.build_expr(lhs);

        // 演算子のコード生成
        self.mgl_builder
            .build_lazy_bin_op(func, item.op.item, Sp::new(&lhs, ast.item.lhs.span), || {
                Ok(Sp::new(self.gen_expr(item.rhs.as_ref())?, item.rhs.span))
            })
            .map(|x| x.into())
    }

    fn gen_un_op_expr(
        &mut self,
        ast: Sp<&ast::UnOpExpr>,
    ) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        let opnd = unwrap_never!(self.gen_expr(item.opnd.as_ref())?);
        let opnd = self.mgl_builder.build_expr(opnd);

        self.mgl_builder
            .build_un_op(item.op.item, opnd, span)
            .map(|x| x.into())
    }

    fn gen_index(&mut self, ast: Sp<&ast::Index>) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        let target = unwrap_never!(self.gen_expr(item.target.as_ref())?);

        // ヒープ上に割り当てられた配列など、targetが配列へのポインタである場合は
        // targetはValueExprになる可能性があるが、
        // ヒープ配列は実装していないためPlaceExprのみを受け付けている
        let target = target
            .try_into()
            .map_err(|ty| Sp::new(CodeGenError::NonIndexableType(ty), span))?;

        let index = unwrap_never!(self.gen_expr(item.index.as_ref())?);
        let index = self.mgl_builder.build_expr(index);

        self.mgl_builder
            .build_index(target, Sp::new(index, item.index.span), span)
            .map(|x| x.into())
    }

    fn gen_literal(&self, ast: Sp<&ast::Literal>) -> Result<ValueExpr<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        match item {
            ast::Literal::Unit(_) => Ok(ValueExpr::unit()),
            ast::Literal::Int(v) => Ok(self.mgl_builder.int(*v)),
            ast::Literal::Float(v) => Ok(self.mgl_builder.double(*v)),
            ast::Literal::Bool(v) => Ok(self.mgl_builder.bool(*v)),
        }
    }

    fn gen_ident(&self, ast: Sp<&String>) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        let var = self
            .variables
            .get(item)
            .ok_or(Sp::new(CodeGenError::UnresolvedName(item.clone()), span))?;
        Ok(var.clone().into())
    }

    fn gen_func_call(
        &mut self,
        ast: Sp<&ast::FuncCall>,
    ) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
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
            let v = unwrap_never!(self.gen_expr(arg.as_ref())?);
            args.push(Sp::new(self.mgl_builder.build_expr(v), arg.span));
        }

        // 関数呼び出し命令を作成
        self.mgl_builder
            .build_call(&func, &args, &item.func_name.item)
            .map(|x| x.into())
    }

    fn gen_if(&mut self, ast: Sp<&ast::If>) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        // 条件式の処理
        // returnするならそれ以降のコード生成を行わない
        let cond = unwrap_never!(self.gen_expr(item.cond.as_ref())?);
        let cond = self.mgl_builder.build_expr(cond);

        match &item.else_ {
            Some(_) => self.gen_if_else(ast, cond),
            None => self.gen_if_without_else(ast, cond),
        }
    }

    fn gen_if_without_else(
        &mut self,
        ast: Sp<&ast::If>,
        cond: ValueExpr<'ctx>,
    ) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        let parent = self.current_fn.as_ref().unwrap().value;

        // 分岐用の基本ブロックを作成
        let then_bb = self.context.append_basic_block(parent, "then");
        let merge_bb = self.context.append_basic_block(parent, "ifcont");

        self.mgl_builder.build_conditional_branch(
            Sp::new(cond, item.cond.span),
            then_bb,
            merge_bb,
        )?;

        // ======================================== then_bbの処理
        self.builder.position_at_end(then_bb);

        // thenのコード生成
        let then_val = self.gen_block(item.then.as_ref())?;

        if let MaybeNever::Value(then_val) = then_val {
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
        Ok(ValueExpr::unit().into())
    }

    fn gen_if_else(
        &mut self,
        ast: Sp<&ast::If>,
        cond: ValueExpr<'ctx>,
    ) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span: _ } = ast;

        let parent = self.current_fn.as_ref().unwrap().value;

        // if式を評価した値を格納する変数
        let mut if_var = None;

        // 分岐用の基本ブロックを作成
        let then_bb = self.context.append_basic_block(parent, "then");
        let else_bb = self.context.append_basic_block(parent, "else");
        let merge_bb = self.context.append_basic_block(parent, "ifcont");

        self.mgl_builder.build_conditional_branch(
            Sp::new(cond, item.cond.span),
            then_bb,
            else_bb,
        )?;

        // ======================================== then_bbの処理
        self.builder.position_at_end(then_bb);

        // thenのコード生成
        let then_val = self.gen_block(item.then.as_ref())?;

        if let MaybeNever::Value(then_val) = then_val {
            let then_val = self.mgl_builder.build_expr(then_val);

            // 変数を作成して格納
            if_var = Some(
                self.mgl_builder
                    .create_variable(parent, &then_val.type_, "ifvalue"),
            );
            self.mgl_builder
                .build_store(if_var.as_ref().unwrap(), Sp::new(&then_val, item.then.span))?;

            self.builder.build_unconditional_branch(merge_bb);
        }

        // ======================================== else_bbの処理
        self.builder.position_at_end(else_bb);

        let else_ = item.else_.as_ref().unwrap();

        // elseのコード生成
        let else_val = self.gen_block(else_.as_ref())?;

        if let MaybeNever::Value(else_val) = else_val {
            let else_val = self.mgl_builder.build_expr(else_val);

            match &if_var {
                Some(var) => {
                    if else_val.type_ != var.type_ {
                        // thenとelseの型が合っていなければエラー
                        return Err(Sp::new(
                            CodeGenError::MismatchedTypes {
                                expected: var.type_.clone(),
                                found: else_val.type_,
                            },
                            else_.span,
                        ));
                    }
                }
                None => {
                    // 変数を作成
                    if_var = Some(self.mgl_builder.create_variable(
                        parent,
                        &else_val.type_,
                        "ifvalue",
                    ));
                }
            }
            // 変数に格納
            self.mgl_builder
                .build_store(if_var.as_ref().unwrap(), Sp::new(&else_val, else_.span))?;

            self.builder.build_unconditional_branch(merge_bb);
        }

        // ======================================== merge_bbの処理

        match &if_var {
            Some(var) => {
                // merge_bbを現在の位置に挿入
                self.builder.position_at_end(merge_bb);
                Ok(self.mgl_builder.build_load(var).into())
            }
            None => {
                // thenとelseの両方の場合でreturnする場合はそれ以降のコード生成を行わない
                merge_bb.remove_from_function().unwrap();
                Ok(MaybeNever::Never)
            }
        }
    }

    fn gen_for(&mut self, ast: Sp<&ast::For>) -> Result<MaybeNever<'ctx>, Sp<CodeGenError>> {
        let Sp { item, span } = ast;

        let parent = self.current_fn.as_ref().unwrap().value;
        let var_name = &item.var_name.item;

        // インデックス変数を作成
        let index_var = self
            .mgl_builder
            .create_variable(parent, &MglType::Double, var_name);
        self.mgl_builder.build_store(
            &index_var,
            Sp::new(&self.mgl_builder.double(0.0).into(), item.var_name.span),
        )?;

        // 同名の変数があればシャドーイングした後、変数を登録
        let old_val = self.variables.remove(var_name);
        self.variables
            .insert(var_name.to_owned(), index_var.clone());

        // 条件判定部分の基本ブロック
        let head_bb = self.context.append_basic_block(parent, "loop_head");

        self.builder.build_unconditional_branch(head_bb);

        // ======================================== head_bbの処理
        self.builder.position_at_end(head_bb);

        let until_val = unwrap_never!(self.gen_expr(item.until.as_ref())?);
        let until_val = self.mgl_builder.build_expr(until_val);

        // until_valの型チェック
        if until_val.type_ != MglType::Double {
            return Err(Sp::new(
                CodeGenError::MismatchedTypes {
                    expected: MglType::Double,
                    found: until_val.type_,
                },
                item.until.span,
            ));
        }

        // インデックスを取得
        let index_val = self.mgl_builder.build_load(&index_var);

        let end_cond = self
            .mgl_builder
            .build_bin_op(BinOp::Lt, &index_val, &until_val, span)?;

        let body_bb = self.context.append_basic_block(parent, "loop_body");
        let after_bb = self.context.append_basic_block(parent, "after_loop");

        self.mgl_builder
            .build_conditional_branch(Sp::new(end_cond, span), body_bb, after_bb)?;

        // ======================================== body_bbの処理
        self.builder.position_at_end(body_bb);

        // 本体のコードを生成
        unwrap_never!(self.gen_block(item.body.as_ref())?);

        // インデックスを取得
        let index_val = self.mgl_builder.build_load(&index_var);

        // インクリメントしてストア
        let next_index_val = self.mgl_builder.build_bin_op(
            BinOp::Add,
            &index_val,
            &self.mgl_builder.double(1.0),
            span,
        )?;

        self.mgl_builder
            .build_store(&index_var, Sp::new(&next_index_val, item.var_name.span))?;

        self.builder.build_unconditional_branch(head_bb);

        // ======================================== after_bbの処理
        self.builder.position_at_end(after_bb);

        // 変数の登録を削除
        self.variables.remove(var_name);

        // シャドーイングした変数があれば戻す
        if let Some(val) = old_val {
            self.variables.insert(var_name.to_owned(), val);
        }

        Ok(ValueExpr::unit().into())
    }
}

pub fn code_gen<'ctx>(
    context: &'ctx Context,
    ast: &ast::Module,
    optimize: bool,
) -> Result<Module<'ctx>, Sp<CodeGenError>> {
    let module = context.create_module("mogral");
    let builder = context.create_builder();
    let mgl_builder = MglBuilder {
        context,
        builder: &builder,
    };
    let mut code_gen = CodeGen::new(&context, &module, &builder, optimize, &mgl_builder);
    code_gen.gen_module(&ast)?;

    Ok(module)
}
