mod error;
mod value;

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::FunctionValue;

use crate::{ast, MglContext, MglModule};

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

    fn gen_module(&mut self, ast: &ast::Module) -> Result<(), CodeGenError> {
        for func in &ast.0 {
            match func {
                ast::Func::Extern(e) => self.gen_func_decl(e),
                ast::Func::FuncDef(f) => self.gen_func_def(f),
            }?;
        }
        Ok(())
    }

    fn gen_func_decl(&mut self, ast: &ast::FuncDecl) -> Result<FunctionValue<'ctx>, CodeGenError> {
        // 関数を作成
        let f64_type = self.context.f64_type();
        let param_types: Vec<BasicMetadataTypeEnum> = vec![f64_type.into(); ast.params.len()];
        let fn_type = f64_type.fn_type(&param_types, false);
        let fn_val = self.module.add_function(&ast.name, fn_type, None);

        // 引数の名前を設定
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(&ast.params[i]);
        }

        Ok(fn_val)
    }

    fn gen_func_def(&mut self, ast: &ast::FuncDef) -> Result<FunctionValue<'ctx>, CodeGenError> {
        let decl = &ast.decl;
        let function = self.gen_func_decl(decl)?;

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
                .create_variable(function, MglType::Double, arg_name);
            self.value_builder.build_store(var, arg)?;

            self.variables.insert(decl.params[i].clone(), var);
        }

        // 関数本体のコード生成
        let body = self.gen_block(&ast.body)?;

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

            Err(CodeGenError::InvalidFunction)
        }
    }

    fn gen_block(&mut self, ast: &ast::Block) -> Result<Option<MglValue<'ctx>>, CodeGenError> {
        // 文のコード生成
        for s in &ast.stmts {
            let res = self.gen_stmt(s)?;
            if res.is_none() {
                // returnするならそれ以降のコード生成を行わない
                return Ok(None);
            }
        }

        // 式があれば生成して評価結果を返し、式が無ければunitを返す
        match &ast.expr {
            Some(e) => self.gen_expr(e),
            None => Ok(Some(MglValue::unit())),
        }
    }

    fn gen_stmt(&mut self, ast: &ast::Stmt) -> Result<Option<()>, CodeGenError> {
        let res = match ast {
            ast::Stmt::Assign(a) => self.gen_assign(a, false),
            ast::Stmt::Expr(e) => self.gen_expr(e),
        }?;
        // 式の評価結果を捨てる
        Ok(res.and(Some(())))
    }

    /// 変数宣言か再代入のコード生成
    /// (引数のsetがtrueの場合は再代入)
    fn gen_assign(
        &mut self,
        ast: &ast::Assign,
        set: bool,
    ) -> Result<Option<MglValue<'ctx>>, CodeGenError> {
        let var = if set {
            // 再代入の場合、変数が存在しない場合にエラー
            *self
                .variables
                .get(&ast.var_name)
                .ok_or(CodeGenError::UnknownVariableName)?
        } else {
            // 新規変数宣言の場合、既に変数が存在する場合にエラー
            if self.variables.contains_key(&ast.var_name) {
                return Err(CodeGenError::VariableAlreadyExists);
            }
            let alloca =
                self.value_builder
                    .create_variable(self.get_func(), MglType::Double, &ast.var_name);
            self.variables.insert(ast.var_name.to_owned(), alloca);
            alloca
        };

        let val = self.gen_expr(&ast.val)?;
        if let Some(val) = val {
            self.value_builder.build_store(var, val)?;
        }
        Ok(val)
    }

    fn gen_expr(&mut self, ast: &ast::Expr) -> Result<Option<MglValue<'ctx>>, CodeGenError> {
        match ast {
            ast::Expr::Set(a) => self.gen_assign(a, true),
            ast::Expr::Return(r) => self.gen_return(r),
            ast::Expr::Op(l, op, r) => self.gen_op_expr(op, l, r),
            ast::Expr::Number(n) => Ok(Some(self.value_builder.double(*n))),
            ast::Expr::Ident(i) => {
                let var = *self
                    .variables
                    .get(i)
                    .ok_or(CodeGenError::UnknownVariableName)?;
                Ok(Some(self.value_builder.build_load(var)))
            }
            ast::Expr::FuncCall(fc) => self.gen_func_call(fc),
            ast::Expr::Block(b) => self.gen_block(b),
            ast::Expr::If(i) => self.gen_if(i),
            ast::Expr::For(f) => self.gen_for(f),
        }
    }

    fn gen_return(&mut self, ast: &ast::Expr) -> Result<Option<MglValue<'ctx>>, CodeGenError> {
        let val = self.gen_expr(ast)?;
        if let Some(val) = val {
            self.value_builder.build_return(val);
        }
        Ok(None)
    }

    fn gen_op_expr(
        &mut self,
        op: &ast::Op,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> Result<Option<MglValue<'ctx>>, CodeGenError> {
        let lhs = self.gen_expr(lhs)?;
        let rhs = self.gen_expr(rhs)?;
        self.value_builder.build_op(*op, lhs, rhs)
    }

    fn gen_func_call(
        &mut self,
        ast: &ast::FuncCall,
    ) -> Result<Option<MglValue<'ctx>>, CodeGenError> {
        let func = self
            .module
            .get_function(&ast.func_name)
            .ok_or(CodeGenError::UnknownFunction)?;

        if ast.args.len() != func.count_params() as usize {
            return Err(CodeGenError::IncorrectNumberOfArguments);
        }

        let mut args = Vec::with_capacity(ast.args.len());

        for arg in &ast.args {
            match self.gen_expr(arg)? {
                Some(v) => args.push(v),
                None => return Ok(None),
            }
        }

        self.value_builder.build_call(func, &args, &ast.func_name)
    }

    fn gen_if(&mut self, ast: &ast::If) -> Result<Option<MglValue<'ctx>>, CodeGenError> {
        // 条件式の処理
        let cond = self.gen_expr(&ast.cond)?;
        let cond = match cond {
            Some(v) => {
                // 型チェック
                if v.type_ != MglType::Bool {
                    return Err(CodeGenError::MismatchedTypes {
                        expected: MglType::Bool,
                        found: v.type_,
                    });
                }
                v
            }
            // returnするならそれ以降のコード生成を行わない
            None => return Ok(None),
        };

        match &ast.else_ {
            Some(_) => self.gen_if_else(ast, cond),
            None => self.gen_if_without_else(ast, cond),
        }
    }

    fn gen_if_without_else(
        &mut self,
        ast: &ast::If,
        cond: MglValue,
    ) -> Result<Option<MglValue<'ctx>>, CodeGenError> {
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
        let then_val = self.gen_block(&ast.then)?;
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
        ast: &ast::If,
        cond: MglValue,
    ) -> Result<Option<MglValue<'ctx>>, CodeGenError> {
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
        let then_val = self.gen_block(&ast.then)?;

        if let Some(then_val) = then_val {
            // 変数を作成して格納
            if_var = Some(
                self.value_builder
                    .create_variable(parent, then_val.type_, "ifvalue"),
            );
            self.value_builder.build_store(if_var.unwrap(), then_val)?;

            self.builder.build_unconditional_branch(merge_bb);
        }

        // ======================================== else_bbの処理
        self.builder.position_at_end(else_bb);

        // elseのコード生成
        let else_val = self.gen_block(ast.else_.as_ref().unwrap())?;

        if let Some(else_val) = else_val {
            match if_var {
                Some(var) => {
                    if else_val.type_ != var.type_ {
                        // thenとelseの型が合っていなければエラー
                        return Err(CodeGenError::MismatchedTypes {
                            expected: var.type_,
                            found: else_val.type_,
                        });
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
            self.value_builder.build_store(if_var.unwrap(), else_val)?;

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

    fn gen_for(&mut self, ast: &ast::For) -> Result<Option<MglValue<'ctx>>, CodeGenError> {
        let parent = self.get_func();

        // インデックス変数を作成
        let index_var = self
            .value_builder
            .create_variable(parent, MglType::Double, &ast.var_name);
        self.value_builder
            .build_store(index_var, self.value_builder.double(0.0))?;

        // 同名の変数があればシャドーイングした後、変数を登録
        let old_val = self.variables.remove(&ast.var_name);
        self.variables.insert(ast.var_name.to_owned(), index_var);

        // 条件判定部分の基本ブロック
        let head_bb = self.context.append_basic_block(parent, "loop_head");

        self.builder.build_unconditional_branch(head_bb);

        // ======================================== head_bbの処理
        self.builder.position_at_end(head_bb);

        let until_val = self.gen_expr(&ast.until)?;
        let until_val = match until_val {
            Some(v) => v,
            None => return Ok(None),
        };

        // インデックスを取得
        let index_val = self.value_builder.build_load(index_var);

        let end_cond = self
            .value_builder
            .build_op(ast::Op::Lt, Some(index_val), Some(until_val))?
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
        if self.gen_block(&ast.body)?.is_none() {
            return Ok(None);
        }

        // インデックスを取得
        let index_val = self.value_builder.build_load(index_var);

        // インクリメントしてストア
        let next_index_val = self
            .value_builder
            .build_op(
                ast::Op::Add,
                Some(index_val),
                Some(self.value_builder.double(1.0)),
            )?
            .unwrap();
        self.value_builder.build_store(index_var, next_index_val)?;

        self.builder.build_unconditional_branch(head_bb);

        // ======================================== after_bbの処理
        self.builder.position_at_end(after_bb);

        // 変数の登録を削除
        self.variables.remove(&ast.var_name);

        // シャドーイングした変数があれば戻す
        if let Some(val) = old_val {
            self.variables.insert(ast.var_name.to_owned(), val);
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
