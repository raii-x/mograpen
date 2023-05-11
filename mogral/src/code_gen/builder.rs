use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, FunctionValue, InstructionValue, PointerValue},
    FloatPredicate, IntPredicate,
};

use crate::{
    pos::{Span, Spanned},
    types::MglType,
    BinOp, LazyBinOp, UnOp,
};

use super::{
    error::CodeGenError,
    value::{Expr, ExprCategory, MaybeNever, MglFunction, PlaceExpr, ValueExpr},
};

/// Exprを扱う命令を作成するための構造体
pub struct MglBuilder<'ctx, 'a> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
}

impl<'ctx, 'a> MglBuilder<'ctx, 'a> {
    /// double型の定数値のValueExprを生成する
    pub fn double(&self, value: f64) -> ValueExpr<'ctx> {
        ValueExpr {
            type_: MglType::Double,
            value: Some(
                self.context
                    .f64_type()
                    .const_float(value)
                    .as_basic_value_enum(),
            ),
        }
    }

    /// bool型の定数値のValueExprを生成する
    pub fn bool(&self, value: bool) -> ValueExpr<'ctx> {
        ValueExpr {
            type_: MglType::Bool,
            value: Some(
                self.context
                    .bool_type()
                    .const_int(if value { 1 } else { 0 }, false)
                    .as_basic_value_enum(),
            ),
        }
    }

    /// MglTypeをinkwellの型に変換する
    pub fn ink_type(&self, type_: &MglType) -> Option<BasicTypeEnum<'ctx>> {
        match type_ {
            MglType::Unit => None,
            MglType::Double => Some(self.context.f64_type().into()),
            MglType::Bool => Some(self.context.bool_type().into()),
            MglType::Array { type_, size } => match self.ink_type(type_) {
                Some(ty) => Some(ty.array_type(*size as u32).into()),
                None => None,
            },
        }
    }

    /// 関数のentryブロックにスタック上の変数を割り当てる命令を作成
    pub fn create_variable(
        &self,
        func: FunctionValue<'ctx>,
        type_: &MglType,
        name: &str,
    ) -> PlaceExpr<'ctx> {
        PlaceExpr {
            type_: type_.clone(),
            alloca: match self.ink_type(&type_) {
                Some(ty) => Some(self.create_entry_block_alloca(func, ty, name)),
                None => None,
            },
        }
    }

    fn create_entry_block_alloca<T: BasicType<'ctx>>(
        &self,
        func: FunctionValue<'ctx>,
        ty: T,
        name: &str,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = func.get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(ty, name)
    }

    /// PlaceExprへのストア命令を作成する
    pub fn build_store(
        &self,
        variable: &PlaceExpr<'ctx>,
        expr: Spanned<&ValueExpr<'ctx>>,
    ) -> Result<Option<InstructionValue<'ctx>>, Spanned<CodeGenError>> {
        // 変数と代入する値の型チェック
        if variable.type_ != expr.item.type_ {
            return Err(Spanned::new(
                CodeGenError::MismatchedTypes {
                    expected: variable.type_.clone(),
                    found: expr.item.type_.clone(),
                },
                expr.span,
            ));
        }

        Ok(match variable.alloca {
            Some(alloca) => Some(self.builder.build_store(alloca, expr.item.value.unwrap())),
            None => None,
        })
    }

    /// PlaceExprからのロード命令を作成する
    pub fn build_load(&self, variable: &PlaceExpr<'ctx>) -> ValueExpr<'ctx> {
        ValueExpr {
            type_: variable.type_.clone(),
            value: match variable.alloca {
                Some(alloca) => Some(self.builder.build_load(
                    self.ink_type(&variable.type_).unwrap(),
                    alloca,
                    "loadtmp",
                )),
                None => None,
            },
        }
    }

    /// 関数引数の読み込み命令を作成
    pub fn build_args(&self, func: &MglFunction<'ctx>) -> Vec<ValueExpr<'ctx>> {
        let mut values = Vec::with_capacity(func.params.len());

        // 関数の引数を変数表に格納
        let mut param_iter = func.value.get_param_iter();
        for type_ in &func.params {
            // 引数値を取得
            let arg_value = ValueExpr {
                type_: type_.clone(),
                // MglTypeに対応するinkwell型がある場合は、inkwellの引数の値を格納
                value: self.ink_type(type_).map(|_| param_iter.next().unwrap()),
            };
            values.push(arg_value);
        }

        values
    }

    /// ExprをValueExprに変換する
    pub fn build_expr(&self, expr: Expr<'ctx>) -> ValueExpr<'ctx> {
        match expr.category {
            ExprCategory::Value(value) => ValueExpr {
                type_: expr.type_,
                value,
            },
            ExprCategory::Place(alloca) => self.build_load(&PlaceExpr {
                type_: expr.type_,
                alloca,
            }),
        }
    }

    /// 配列のインデックス参照命令を作成
    pub fn build_index(
        &self,
        target: PlaceExpr<'ctx>,
        index: Spanned<ValueExpr<'ctx>>,
        span: Span,
    ) -> Result<PlaceExpr<'ctx>, Spanned<CodeGenError>> {
        // targetの型チェックと要素型の取得
        let elm_ty = match &target.type_ {
            MglType::Array { type_, size: _ } => type_.as_ref(),
            _ => {
                // 配列型でなければエラー
                return Err(Spanned::new(
                    CodeGenError::NonIndexableType(target.type_),
                    span,
                ));
            }
        };

        // インデックスの型チェック
        if index.item.type_ != MglType::Double {
            return Err(Spanned::new(
                CodeGenError::InvalidIndexType(index.item.type_),
                index.span,
            ));
        }

        // インデックスのdoubleをi64に変換
        let index = self.builder.build_float_to_unsigned_int(
            index.item.value.unwrap().into_float_value(),
            self.context.i64_type(),
            "index",
        );

        // GEP命令用の配列を作成
        let index_array = [self.context.i64_type().const_int(0, false), index];

        // 配列の要素を取得
        Ok(PlaceExpr {
            type_: elm_ty.clone(),
            alloca: match self.ink_type(&target.type_) {
                Some(array_ty) => unsafe {
                    self.builder
                        .build_in_bounds_gep(
                            array_ty,
                            target.alloca.unwrap(),
                            &index_array,
                            "indextmp",
                        )
                        .into()
                },
                None => None,
            },
        })
    }

    /// ValueExprを戻り値とするreturn命令を作成する
    pub fn build_return(
        &self,
        ret_type: &MglType,
        expr: Spanned<&ValueExpr<'ctx>>,
    ) -> Result<InstructionValue<'ctx>, Spanned<CodeGenError>> {
        // 戻り値の型チェック
        if &expr.item.type_ != ret_type {
            return Err(Spanned::new(
                CodeGenError::MismatchedTypes {
                    expected: ret_type.clone(),
                    found: expr.item.type_.clone(),
                },
                expr.span,
            ));
        }

        // return命令を作成
        Ok(match expr.item.value {
            Some(value) => self.builder.build_return(Some(&value)),
            None => self.builder.build_return(None),
        })
    }

    /// 関数呼び出し命令を作成する
    pub fn build_call(
        &self,
        function: &MglFunction<'ctx>,
        args: &[Spanned<ValueExpr<'ctx>>],
        name: &str,
    ) -> Result<ValueExpr<'ctx>, Spanned<CodeGenError>> {
        // inkwellの引数のVecを作成
        let mut ink_args = Vec::with_capacity(args.len());
        for (param_ty, arg) in function.params.iter().zip(args) {
            // 引数の型チェック
            if arg.item.type_ != *param_ty {
                return Err(Spanned::new(
                    CodeGenError::MismatchedTypes {
                        expected: param_ty.clone(),
                        found: arg.item.type_.clone(),
                    },
                    arg.span,
                ));
            }

            // 引数に追加
            if let Some(val) = arg.item.value {
                ink_args.push(val.into());
            }
        }

        // 関数呼び出し命令を作成
        let call_site = self.builder.build_call(function.value, &ink_args, name);

        // 関数の戻り値を返す
        Ok(ValueExpr {
            type_: function.ret_type.clone(),
            value: match self.ink_type(&function.ret_type) {
                Some(_) => Some(call_site.try_as_basic_value().left().unwrap()),
                None => None,
            },
        })
    }

    /// 二項演算命令を作成する
    pub fn build_bin_op(
        &self,
        op: BinOp,
        lhs: &ValueExpr<'ctx>,
        rhs: &ValueExpr<'ctx>,
        span: Span,
    ) -> Result<ValueExpr<'ctx>, Spanned<CodeGenError>> {
        if lhs.type_ != rhs.type_ {
            return Err(Spanned::new(
                CodeGenError::InvalidBinaryOperandTypes {
                    op,
                    lhs: lhs.type_.clone(),
                    rhs: rhs.type_.clone(),
                },
                span,
            ));
        }

        // 被演算子の型が対応していなければNoneとしている
        let val = match lhs.type_ {
            MglType::Double => {
                let lhs_v = lhs.value.unwrap().into_float_value();
                let rhs_v = rhs.value.unwrap().into_float_value();

                match op {
                    BinOp::Lt => Some(ValueExpr {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::ULT, lhs_v, rhs_v, "lttmp")
                                .into(),
                        ),
                    }),
                    BinOp::Gt => Some(ValueExpr {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UGT, lhs_v, rhs_v, "gttmp")
                                .into(),
                        ),
                    }),
                    BinOp::Leq => Some(ValueExpr {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::ULE, lhs_v, rhs_v, "letmp")
                                .into(),
                        ),
                    }),
                    BinOp::Geq => Some(ValueExpr {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UGE, lhs_v, rhs_v, "getmp")
                                .into(),
                        ),
                    }),
                    BinOp::Eq => Some(ValueExpr {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UEQ, lhs_v, rhs_v, "eqtmp")
                                .into(),
                        ),
                    }),
                    BinOp::Neq => Some(ValueExpr {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UNE, lhs_v, rhs_v, "netmp")
                                .into(),
                        ),
                    }),
                    BinOp::Add => Some(ValueExpr {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_add(lhs_v, rhs_v, "addtmp").into()),
                    }),
                    BinOp::Sub => Some(ValueExpr {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_sub(lhs_v, rhs_v, "subtmp").into()),
                    }),
                    BinOp::Mul => Some(ValueExpr {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_mul(lhs_v, rhs_v, "multmp").into()),
                    }),
                    BinOp::Div => Some(ValueExpr {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_div(lhs_v, rhs_v, "divtmp").into()),
                    }),
                    BinOp::Rem => Some(ValueExpr {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_rem(lhs_v, rhs_v, "remtmp").into()),
                    }),
                }
            }
            MglType::Bool => {
                let lhs_v = lhs.value.unwrap().into_int_value();
                let rhs_v = rhs.value.unwrap().into_int_value();

                match op {
                    BinOp::Eq => Some(ValueExpr {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_int_compare(IntPredicate::EQ, lhs_v, rhs_v, "eqtmp")
                                .into(),
                        ),
                    }),
                    BinOp::Neq => Some(ValueExpr {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_int_compare(IntPredicate::NE, lhs_v, rhs_v, "eqtmp")
                                .into(),
                        ),
                    }),
                    _ => None,
                }
            }
            MglType::Unit => match op {
                BinOp::Eq => Some(self.bool(true)),
                BinOp::Neq => Some(self.bool(false)),
                _ => None,
            },
            MglType::Array { .. } => None,
        };

        match val {
            Some(v) => Ok(v),
            None => Err(Spanned::new(
                CodeGenError::InvalidBinaryOperandTypes {
                    op,
                    lhs: lhs.type_.clone(),
                    rhs: rhs.type_.clone(),
                },
                span,
            )),
        }
    }

    /// 遅延評価を行う二項演算命令を作成する
    pub fn build_lazy_bin_op<F>(
        &self,
        func: FunctionValue<'ctx>,
        op: LazyBinOp,
        lhs: Spanned<&ValueExpr<'ctx>>,
        gen_rhs: F,
    ) -> Result<ValueExpr<'ctx>, Spanned<CodeGenError>>
    where
        F: FnOnce() -> Result<Spanned<MaybeNever<'ctx>>, Spanned<CodeGenError>>,
    {
        // ======================================== 左辺の基本ブロックの処理

        // 左辺がboolでなければエラー
        if lhs.item.type_ != MglType::Bool {
            return Err(Spanned::new(
                CodeGenError::MismatchedTypes {
                    expected: MglType::Bool,
                    found: lhs.item.type_.clone(),
                },
                lhs.span,
            ));
        }

        // 演算を評価した値を格納する変数
        let eval_var = self.create_variable(func, &MglType::Bool, "lazy_bin_value");

        // 変数に左辺の結果を格納
        self.builder
            .build_store(eval_var.alloca.unwrap(), lhs.item.value.unwrap());

        // 基本ブロックを作成
        let rhs_bb = self.context.append_basic_block(func, "lazy_bin_rhs");
        let merge_bb = self.context.append_basic_block(func, "lazy_bin_merge");

        // 分岐を作成
        match op {
            LazyBinOp::And => {
                self.builder.build_conditional_branch(
                    lhs.item.value.unwrap().into_int_value(),
                    rhs_bb,
                    merge_bb,
                );
            }
            LazyBinOp::Or => {
                self.builder.build_conditional_branch(
                    lhs.item.value.unwrap().into_int_value(),
                    merge_bb,
                    rhs_bb,
                );
            }
        }

        // ======================================== rhs_bbの処理

        // 右辺の基本ブロックから挿入開始
        self.builder.position_at_end(rhs_bb);

        // 右辺のコード生成
        let rhs = gen_rhs()?;

        // 右辺でreturnしない場合
        if let MaybeNever::Value(rhs_v) = rhs.item {
            // 右辺がboolでなければエラー
            if rhs_v.type_ != MglType::Bool {
                return Err(Spanned::new(
                    CodeGenError::MismatchedTypes {
                        expected: MglType::Bool,
                        found: rhs_v.type_,
                    },
                    rhs.span,
                ));
            }

            let rhs_v = self.build_expr(rhs_v);

            // 変数に右辺の結果を格納
            self.builder
                .build_store(eval_var.alloca.unwrap(), rhs_v.value.unwrap());

            // 合流用の基本ブロックにジャンプ
            self.builder.build_unconditional_branch(merge_bb);
        }

        // ======================================== merge_bbの処理

        // 合流用の基本ブロックから挿入開始
        self.builder.position_at_end(merge_bb);

        // 変数から評価した値を読み出して返す
        Ok(self.build_load(&eval_var))
    }

    /// 単項演算命令を作成する
    pub fn build_un_op(
        &self,
        op: UnOp,
        opnd: ValueExpr<'ctx>,
        span: Span,
    ) -> Result<ValueExpr<'ctx>, Spanned<CodeGenError>> {
        match opnd.type_ {
            MglType::Double => {
                let opnd = opnd.value.unwrap().into_float_value();

                match op {
                    UnOp::Neg => Some(ValueExpr {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_neg(opnd, "negtmp").into()),
                    }),
                    _ => None,
                }
            }
            MglType::Bool => {
                let opnd = opnd.value.unwrap().into_int_value();

                match op {
                    UnOp::Not => Some(ValueExpr {
                        type_: MglType::Bool,
                        value: Some(self.builder.build_not(opnd, "nottmp").into()),
                    }),
                    _ => None,
                }
            }
            MglType::Unit => None,
            MglType::Array { .. } => None,
        }
        .ok_or(Spanned::new(
            CodeGenError::InvalidUnaryOperandType {
                op,
                type_: opnd.type_,
            },
            span,
        ))
    }

    pub fn build_conditional_branch(
        &self,
        cond: Spanned<ValueExpr<'ctx>>,
        then_bb: BasicBlock<'ctx>,
        else_bb: BasicBlock<'ctx>,
    ) -> Result<InstructionValue<'ctx>, Spanned<CodeGenError>> {
        // 型チェック
        if cond.item.type_ != MglType::Bool {
            return Err(Spanned::new(
                CodeGenError::MismatchedTypes {
                    expected: MglType::Bool,
                    found: cond.item.type_,
                },
                cond.span,
            ));
        }

        let cond = cond.item.value.unwrap().into_int_value();
        Ok(self
            .builder
            .build_conditional_branch(cond, then_bb, else_bb))
    }
}
