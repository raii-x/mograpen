mod error;

use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{FloatValue, FunctionValue, IntValue, PointerValue};
use inkwell::FloatPredicate;

use crate::{ast, MglContext, MglModule};

use self::error::CodeGenError;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    fpm: Option<PassManager<FunctionValue<'ctx>>>,
    variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    fn new(context: &'ctx Context, optimize: bool) -> Self {
        let module = context.create_module("mogral");

        let fpm = if optimize {
            // 最適化を行う場合はFPMを作成
            let fpm = PassManager::create(&module);

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
            builder: context.create_builder(),
            fpm,
            variables: HashMap::new(),
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

        // 変数表を作成
        self.variables.reserve(decl.params.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = &decl.params[i];
            let alloca = self.create_entry_block_alloca(function, arg_name);

            self.builder.build_store(alloca, arg);

            self.variables.insert(decl.params[i].clone(), alloca);
        }

        // 関数本体のコード生成
        let body = self.gen_block(&ast.body)?;

        if let Some(body) = body {
            // return文が無いパスがある場合はここでreturnを作成
            self.builder.build_return(Some(&body));
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

    fn gen_block(&mut self, ast: &ast::Block) -> Result<Option<FloatValue<'ctx>>, CodeGenError> {
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
            // TODO: unit型を返す
            None => Ok(Some(self.context.f64_type().get_undef())),
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
    ) -> Result<Option<FloatValue<'ctx>>, CodeGenError> {
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
            let alloca = self.create_entry_block_alloca(self.get_func(), &ast.var_name);
            self.variables.insert(ast.var_name.to_owned(), alloca);
            alloca
        };

        let val = self.gen_expr(&ast.val)?;
        if let Some(val) = val {
            self.builder.build_store(var, val);
        }
        Ok(val)
    }

    fn gen_expr(&mut self, ast: &ast::Expr) -> Result<Option<FloatValue<'ctx>>, CodeGenError> {
        match ast {
            ast::Expr::Set(a) => self.gen_assign(a, true),
            ast::Expr::Return(r) => self.gen_return(r),
            ast::Expr::Op(l, op, r) => self.gen_op_expr(op, l, r),
            ast::Expr::Number(n) => Ok(Some(self.context.f64_type().const_float(*n))),
            ast::Expr::Ident(i) => {
                let var = *self
                    .variables
                    .get(i)
                    .ok_or(CodeGenError::UnknownVariableName)?;
                Ok(Some(
                    self.builder
                        .build_load(self.context.f64_type(), var, i) // inkwellではgetAllocatedTypeが使えない？
                        .into_float_value(),
                ))
            }
            ast::Expr::FuncCall(fc) => self.gen_func_call(fc),
            ast::Expr::Block(b) => self.gen_block(b),
            ast::Expr::If(i) => self.gen_if(i),
            ast::Expr::For(f) => self.gen_for(f),
        }
    }

    fn gen_return(&mut self, ast: &ast::Expr) -> Result<Option<FloatValue<'ctx>>, CodeGenError> {
        let val = self.gen_expr(ast)?;
        if let Some(val) = val {
            self.builder.build_return(Some(&val));
        }
        Ok(None)
    }

    fn gen_op_expr(
        &mut self,
        op: &ast::Op,
        lhs: &ast::Expr,
        rhs: &ast::Expr,
    ) -> Result<Option<FloatValue<'ctx>>, CodeGenError> {
        let lhs = self.gen_expr(lhs)?;
        let lhs = match lhs {
            Some(v) => v,
            None => return Ok(None),
        };

        let rhs = self.gen_expr(rhs)?;
        let rhs = match rhs {
            Some(v) => v,
            None => return Ok(None),
        };

        Ok(Some(match op {
            ast::Op::Lt => self.cmp_to_f64(self.builder.build_float_compare(
                FloatPredicate::ULT,
                lhs,
                rhs,
                "lttmp",
            )),
            ast::Op::Gt => self.cmp_to_f64(self.builder.build_float_compare(
                FloatPredicate::UGT,
                lhs,
                rhs,
                "gttmp",
            )),
            ast::Op::Leq => self.cmp_to_f64(self.builder.build_float_compare(
                FloatPredicate::ULE,
                lhs,
                rhs,
                "letmp",
            )),
            ast::Op::Geq => self.cmp_to_f64(self.builder.build_float_compare(
                FloatPredicate::UGE,
                lhs,
                rhs,
                "getmp",
            )),
            ast::Op::Eq => self.cmp_to_f64(self.builder.build_float_compare(
                FloatPredicate::UEQ,
                lhs,
                rhs,
                "eqtmp",
            )),
            ast::Op::Neq => self.cmp_to_f64(self.builder.build_float_compare(
                FloatPredicate::UNE,
                lhs,
                rhs,
                "netmp",
            )),
            ast::Op::Add => self.builder.build_float_add(lhs, rhs, "addtmp"),
            ast::Op::Sub => self.builder.build_float_sub(lhs, rhs, "subtmp"),
            ast::Op::Mul => self.builder.build_float_mul(lhs, rhs, "multmp"),
            ast::Op::Div => self.builder.build_float_div(lhs, rhs, "divtmp"),
        }))
    }

    /// 比較演算の結果のIntValueをFloatValueに変換する
    fn cmp_to_f64(&self, cmp: IntValue<'ctx>) -> FloatValue<'ctx> {
        self.builder
            .build_unsigned_int_to_float(cmp, self.context.f64_type(), "booltmp")
    }

    fn gen_func_call(
        &mut self,
        ast: &ast::FuncCall,
    ) -> Result<Option<FloatValue<'ctx>>, CodeGenError> {
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
                Some(v) => args.push(v.into()),
                None => return Ok(None),
            }
        }

        match self
            .builder
            .build_call(func, &args, &ast.func_name)
            .try_as_basic_value()
            .left()
        {
            Some(val) => Ok(Some(val.into_float_value())),
            None => Err(CodeGenError::InvalidCall),
        }
    }

    fn gen_if(&mut self, ast: &ast::If) -> Result<Option<FloatValue<'ctx>>, CodeGenError> {
        let zero_const = self.context.f64_type().const_float(0.0);

        // 条件式の処理
        let cond = self.gen_expr(&ast.cond)?;
        let cond = match cond {
            Some(v) => {
                self.builder
                    .build_float_compare(FloatPredicate::ONE, v, zero_const, "ifcond")
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
        cond: IntValue,
    ) -> Result<Option<FloatValue<'ctx>>, CodeGenError> {
        let parent = self.get_func();

        // 分岐用の基本ブロックを作成
        let then_bb = self.context.append_basic_block(parent, "then");
        let merge_bb = self.context.append_basic_block(parent, "ifcont");

        self.builder
            .build_conditional_branch(cond, then_bb, merge_bb);

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
        // TODO: unit型を返す
        Ok(Some(self.context.f64_type().get_undef()))
    }

    fn gen_if_else(
        &mut self,
        ast: &ast::If,
        cond: IntValue,
    ) -> Result<Option<FloatValue<'ctx>>, CodeGenError> {
        let parent = self.get_func();

        // ifを評価した値を格納する変数を作成
        let if_alloca = self.create_entry_block_alloca(parent, "ifvalue");

        // 分岐用の基本ブロックを作成
        let then_bb = self.context.append_basic_block(parent, "then");
        let else_bb = self.context.append_basic_block(parent, "else");
        let merge_bb = self.context.append_basic_block(parent, "ifcont");

        self.builder
            .build_conditional_branch(cond, then_bb, else_bb);

        // ======================================== then_bbの処理
        self.builder.position_at_end(then_bb);

        // thenのコード生成
        let then_val = self.gen_block(&ast.then)?;
        if let Some(val) = then_val {
            self.builder.build_store(if_alloca, val);
            self.builder.build_unconditional_branch(merge_bb);
        }

        // ======================================== else_bbの処理
        self.builder.position_at_end(else_bb);

        // elseのコード生成
        let else_val = self.gen_block(ast.else_.as_ref().unwrap())?;
        if let Some(val) = else_val {
            self.builder.build_store(if_alloca, val);
            self.builder.build_unconditional_branch(merge_bb);
        }

        // ======================================== merge_bbの処理

        if then_val.is_none() && else_val.is_none() {
            // ifとthenの両方の場合でreturnする場合はそれ以降のコード生成を行わない
            merge_bb.remove_from_function().unwrap();
            Ok(None)
        } else {
            // merge_bbを現在の位置に挿入
            self.builder.position_at_end(merge_bb);
            let val = self
                .builder
                .build_load(self.context.f64_type(), if_alloca, "ifvalue");
            Ok(Some(val.into_float_value()))
        }
    }

    fn gen_for(&mut self, ast: &ast::For) -> Result<Option<FloatValue<'ctx>>, CodeGenError> {
        let parent = self.get_func();

        // インデックス変数を作成
        let index_alloca = self.create_entry_block_alloca(parent, &ast.var_name);
        self.builder
            .build_store(index_alloca, self.context.f64_type().const_zero());

        // 同名の変数があればシャドーイングした後、変数を登録
        let old_val = self.variables.remove(&ast.var_name);
        self.variables.insert(ast.var_name.to_owned(), index_alloca);

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
        let index_val = self
            .builder
            .build_load(self.context.f64_type(), index_alloca, &ast.var_name)
            .into_float_value();

        let end_cond =
            self.builder
                .build_float_compare(FloatPredicate::OLT, index_val, until_val, "loopcond");

        let body_bb = self.context.append_basic_block(parent, "loop_body");
        let after_bb = self.context.append_basic_block(parent, "after_loop");

        self.builder
            .build_conditional_branch(end_cond, body_bb, after_bb);

        // ======================================== body_bbの処理
        self.builder.position_at_end(body_bb);

        // 本体のコードを生成
        if self.gen_block(&ast.body)?.is_none() {
            return Ok(None);
        }

        // インデックスを取得
        let index_val = self
            .builder
            .build_load(self.context.f64_type(), index_alloca, &ast.var_name)
            .into_float_value();

        // インクリメントしてストア
        let next_index_val = self.builder.build_float_add(
            index_val,
            self.context.f64_type().const_float(1.0),
            "nextvar",
        );
        self.builder.build_store(index_alloca, next_index_val);

        self.builder.build_unconditional_branch(head_bb);

        // ======================================== after_bbの処理
        self.builder.position_at_end(after_bb);

        // 変数の登録を削除
        self.variables.remove(&ast.var_name);

        // シャドーイングした変数があれば戻す
        if let Some(val) = old_val {
            self.variables.insert(ast.var_name.to_owned(), val);
        }

        // TODO: unit型を返す
        Ok(Some(self.context.f64_type().get_undef()))
    }

    /// builderが現在処理中の関数を得る
    fn get_func(&self) -> FunctionValue<'ctx> {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
    }

    /// 関数のentryブロックに新しいスタックアロケーション命令を作成
    fn create_entry_block_alloca(
        &self,
        func: FunctionValue<'ctx>,
        name: &str,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = func.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name)
    }
}

pub fn code_gen<'ctx>(
    context: &'ctx MglContext,
    ast: &ast::Module,
    optimize: bool,
) -> Result<MglModule<'ctx>, Box<dyn std::error::Error>> {
    let mut code_gen = CodeGen::new(&context.0, optimize);
    code_gen.gen_module(&ast)?;

    Ok(MglModule(code_gen.module))
}
