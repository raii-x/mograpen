use inkwell::{
    builder::Builder,
    context::Context,
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue, InstructionValue, PointerValue},
    FloatPredicate, IntPredicate,
};

use crate::{
    pos::{Span, Spanned},
    types::MglType,
    BinOp, LazyBinOp, UnOp,
};

use super::error::CodeGenError;

/// MograLで使用する値
#[derive(Clone, Copy)]
pub struct MglValue<'ctx> {
    pub type_: MglType,
    pub value: Option<BasicValueEnum<'ctx>>,
}

impl<'ctx> MglValue<'ctx> {
    pub fn unit() -> Self {
        Self {
            type_: MglType::Unit,
            value: None,
        }
    }
}

/// MograLで使用する変数
#[derive(Clone, Copy)]
pub struct MglVariable<'ctx> {
    pub type_: MglType,
    pub alloca: Option<PointerValue<'ctx>>,
}

/// MglValueを扱う命令を作成するための構造体
pub struct MglValueBuilder<'ctx, 'a> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
}

impl<'ctx, 'a> MglValueBuilder<'ctx, 'a> {
    /// double型の定数値のMglValueを生成する
    pub fn double(&self, value: f64) -> MglValue<'ctx> {
        MglValue {
            type_: MglType::Double,
            value: Some(
                self.context
                    .f64_type()
                    .const_float(value)
                    .as_basic_value_enum(),
            ),
        }
    }

    pub fn bool(&self, value: bool) -> MglValue<'ctx> {
        MglValue {
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
    pub fn ink_type(&self, type_: MglType) -> Option<BasicTypeEnum<'ctx>> {
        match type_ {
            MglType::Unit => None,
            MglType::Double => Some(self.context.f64_type().into()),
            MglType::Bool => Some(self.context.bool_type().into()),
        }
    }

    /// 関数のentryブロックにスタック上の変数を割り当てる命令を作成
    pub fn create_variable(
        &self,
        func: FunctionValue<'ctx>,
        type_: MglType,
        name: &str,
    ) -> MglVariable<'ctx> {
        match type_ {
            MglType::Unit => MglVariable {
                type_,
                alloca: None,
            },
            MglType::Double => MglVariable {
                type_,
                alloca: Some(self.create_entry_block_alloca(func, self.context.f64_type(), name)),
            },
            MglType::Bool => MglVariable {
                type_,
                alloca: Some(self.create_entry_block_alloca(func, self.context.bool_type(), name)),
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

    /// MglVariableへのストア命令を作成する
    pub fn build_store(
        &self,
        variable: MglVariable<'ctx>,
        value: Spanned<MglValue<'ctx>>,
    ) -> Result<Option<InstructionValue<'ctx>>, Spanned<CodeGenError>> {
        // 変数と代入する値の型チェック
        if variable.type_ != value.item.type_ {
            return Err(Spanned::new(
                CodeGenError::MismatchedTypes {
                    expected: variable.type_,
                    found: value.item.type_,
                },
                value.span,
            ));
        }

        Ok(match variable.alloca {
            Some(alloca) => Some(self.builder.build_store(alloca, value.item.value.unwrap())),
            None => None,
        })
    }

    /// MglVariableからのロード命令を作成する
    pub fn build_load(&self, variable: MglVariable<'ctx>) -> MglValue<'ctx> {
        MglValue {
            type_: variable.type_,
            value: match variable.alloca {
                Some(alloca) => Some(self.builder.build_load(
                    self.ink_type(variable.type_).unwrap(),
                    alloca,
                    "loadtmp",
                )),
                None => None,
            },
        }
    }

    /// MglValueを返すreturn命令を作成する
    pub fn build_return(
        &self,
        ret_type: MglType,
        value: Spanned<MglValue<'ctx>>,
    ) -> Result<InstructionValue<'ctx>, Spanned<CodeGenError>> {
        // 戻り値の型チェック
        if value.item.type_ != ret_type {
            return Err(Spanned::new(
                CodeGenError::MismatchedTypes {
                    expected: ret_type,
                    found: value.item.type_,
                },
                value.span,
            ));
        }

        // return命令を作成
        Ok(match value.item.value {
            Some(value) => self.builder.build_return(Some(&value)),
            None => self.builder.build_return(None),
        })
    }

    /// 関数呼び出し命令を作成する
    pub fn build_call(
        &self,
        function: &MglFunction<'ctx>,
        args: &[Spanned<MglValue<'ctx>>],
        name: &str,
    ) -> Result<Option<MglValue<'ctx>>, Spanned<CodeGenError>> {
        // inkwellの引数のVecを作成
        let mut ink_args = Vec::with_capacity(args.len());
        for (param_ty, arg) in function.params.iter().zip(args) {
            // 引数の型チェック
            if arg.item.type_ != *param_ty {
                return Err(Spanned::new(
                    CodeGenError::MismatchedTypes {
                        expected: *param_ty,
                        found: arg.item.type_,
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
        Ok(Some(MglValue {
            type_: function.ret_type,
            value: match self.ink_type(function.ret_type) {
                Some(_) => Some(call_site.try_as_basic_value().left().unwrap()),
                None => None,
            },
        }))
    }

    /// 二項演算命令を作成する
    pub fn build_bin_op(
        &self,
        op: BinOp,
        lhs: Option<MglValue<'ctx>>,
        rhs: Option<MglValue<'ctx>>,
        span: Span,
    ) -> Result<Option<MglValue<'ctx>>, Spanned<CodeGenError>> {
        match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => self
                .build_bin_op_inner(op, lhs, rhs)
                .map(Some)
                .map_err(|e| Spanned::new(e, span)),
            _ => Ok(None),
        }
    }

    fn build_bin_op_inner(
        &self,
        op: BinOp,
        lhs: MglValue<'ctx>,
        rhs: MglValue<'ctx>,
    ) -> Result<MglValue<'ctx>, CodeGenError> {
        if lhs.type_ != rhs.type_ {
            return Err(CodeGenError::InvalidBinaryOperandTypes {
                op,
                lhs: lhs.type_,
                rhs: rhs.type_,
            });
        }

        match lhs.type_ {
            MglType::Double => {
                let lhs_v = lhs.value.unwrap().into_float_value();
                let rhs_v = rhs.value.unwrap().into_float_value();

                match op {
                    BinOp::Lt => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::ULT, lhs_v, rhs_v, "lttmp")
                                .into(),
                        ),
                    }),
                    BinOp::Gt => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UGT, lhs_v, rhs_v, "gttmp")
                                .into(),
                        ),
                    }),
                    BinOp::Leq => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::ULE, lhs_v, rhs_v, "letmp")
                                .into(),
                        ),
                    }),
                    BinOp::Geq => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UGE, lhs_v, rhs_v, "getmp")
                                .into(),
                        ),
                    }),
                    BinOp::Eq => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UEQ, lhs_v, rhs_v, "eqtmp")
                                .into(),
                        ),
                    }),
                    BinOp::Neq => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UNE, lhs_v, rhs_v, "netmp")
                                .into(),
                        ),
                    }),
                    BinOp::Add => Some(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_add(lhs_v, rhs_v, "addtmp").into()),
                    }),
                    BinOp::Sub => Some(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_sub(lhs_v, rhs_v, "subtmp").into()),
                    }),
                    BinOp::Mul => Some(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_mul(lhs_v, rhs_v, "multmp").into()),
                    }),
                    BinOp::Div => Some(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_div(lhs_v, rhs_v, "divtmp").into()),
                    }),
                }
            }
            MglType::Bool => {
                let lhs_v = lhs.value.unwrap().into_int_value();
                let rhs_v = rhs.value.unwrap().into_int_value();

                match op {
                    BinOp::Eq => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_int_compare(IntPredicate::EQ, lhs_v, rhs_v, "eqtmp")
                                .into(),
                        ),
                    }),
                    BinOp::Neq => Some(MglValue {
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
        }
        .ok_or(CodeGenError::InvalidBinaryOperandTypes {
            op,
            lhs: lhs.type_,
            rhs: rhs.type_,
        })
    }

    /// 遅延評価を行う二項演算命令を作成する
    pub fn build_lazy_bin_op<F>(
        &self,
        func: FunctionValue<'ctx>,
        op: LazyBinOp,
        lhs: Spanned<Option<MglValue<'ctx>>>,
        gen_rhs: F,
    ) -> Result<Option<MglValue<'ctx>>, Spanned<CodeGenError>>
    where
        F: FnOnce() -> Result<Spanned<Option<MglValue<'ctx>>>, Spanned<CodeGenError>>,
    {
        // ======================================== 左辺の基本ブロックの処理

        let lhs_v = match lhs.item {
            Some(v) => v,
            None => {
                // 左辺でreturnする場合は右辺を評価しない
                return Ok(None);
            }
        };

        // 左辺がboolでなければエラー
        if lhs_v.type_ != MglType::Bool {
            return Err(Spanned::new(
                CodeGenError::MismatchedTypes {
                    expected: MglType::Bool,
                    found: lhs_v.type_,
                },
                lhs.span,
            ));
        }

        // 演算を評価した値を格納する変数
        let eval_var = self.create_variable(func, MglType::Bool, "lazy_bin_value");

        // 変数に左辺の結果を格納
        self.builder
            .build_store(eval_var.alloca.unwrap(), lhs_v.value.unwrap());

        // 基本ブロックを作成
        let rhs_bb = self.context.append_basic_block(func, "lazy_bin_rhs");
        let merge_bb = self.context.append_basic_block(func, "lazy_bin_merge");

        // 分岐を作成
        match op {
            LazyBinOp::And => {
                self.builder.build_conditional_branch(
                    lhs_v.value.unwrap().into_int_value(),
                    rhs_bb,
                    merge_bb,
                );
            }
            LazyBinOp::Or => {
                self.builder.build_conditional_branch(
                    lhs_v.value.unwrap().into_int_value(),
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
        if let Some(rhs_v) = rhs.item {
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
        Ok(Some(self.build_load(eval_var)))
    }

    /// 単項演算命令を作成する
    pub fn build_un_op(
        &self,
        op: UnOp,
        opnd: Option<MglValue<'ctx>>,
        span: Span,
    ) -> Result<Option<MglValue<'ctx>>, Spanned<CodeGenError>> {
        match opnd {
            Some(opnd) => self
                .build_un_op_inner(op, opnd)
                .map(Some)
                .map_err(|e| Spanned::new(e, span)),
            _ => Ok(None),
        }
    }

    fn build_un_op_inner(
        &self,
        op: UnOp,
        opnd: MglValue<'ctx>,
    ) -> Result<MglValue<'ctx>, CodeGenError> {
        match opnd.type_ {
            MglType::Double => {
                let opnd = opnd.value.unwrap().into_float_value();

                match op {
                    UnOp::Neg => Some(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_neg(opnd, "negtmp").into()),
                    }),
                    _ => None,
                }
            }
            MglType::Bool => {
                let opnd = opnd.value.unwrap().into_int_value();

                match op {
                    UnOp::Not => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(self.builder.build_not(opnd, "nottmp").into()),
                    }),
                    _ => None,
                }
            }
            MglType::Unit => None,
        }
        .ok_or(CodeGenError::InvalidUnaryOperandType {
            op,
            type_: opnd.type_,
        })
    }
}

#[derive(Clone)]
pub struct MglFunction<'ctx> {
    pub params: Vec<MglType>,
    pub ret_type: MglType,
    pub value: FunctionValue<'ctx>,
}
