use inkwell::{
    builder::Builder,
    context::Context,
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue, InstructionValue, PointerValue},
    FloatPredicate,
};

use crate::{
    op::Op,
    pos::{Span, Spanned},
};

use super::{error::CodeGenError, types::MglType};

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

/// MglValueを扱う命令を作成ための構造体
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
    pub fn build_return(&self, value: MglValue<'ctx>) -> InstructionValue<'ctx> {
        match value.value {
            Some(value) => self.builder.build_return(Some(&value)),
            None => self.builder.build_return(None),
        }
    }

    /// 関数呼び出し命令を作成する
    pub fn build_call(
        &self,
        function: FunctionValue<'ctx>,
        args: &[Spanned<MglValue<'ctx>>],
        name: &str,
        span: Span,
    ) -> Result<Option<MglValue<'ctx>>, Spanned<CodeGenError>> {
        let mut ink_args = Vec::new();
        for arg in args {
            // 引数の型チェック
            // TODO: Double型以外を渡せるようにする
            if arg.item.type_ != MglType::Double {
                return Err(Spanned::new(
                    CodeGenError::MismatchedTypes {
                        expected: MglType::Double,
                        found: arg.item.type_,
                    },
                    arg.span,
                ));
            }

            if let Some(val) = arg.item.value {
                ink_args.push(val.into());
            }
        }

        match self
            .builder
            .build_call(function, &ink_args, name)
            .try_as_basic_value()
            .left()
        {
            // TODO: Double型以外の戻り値に対応する
            Some(val) => Ok(Some(MglValue {
                type_: MglType::Double,
                value: Some(val),
            })),
            None => Err(Spanned::new(CodeGenError::InvalidCall, span)),
        }
    }

    /// 演算命令を作成する
    pub fn build_op(
        &self,
        op: Op,
        lhs: Option<MglValue<'ctx>>,
        rhs: Option<MglValue<'ctx>>,
        span: Span,
    ) -> Result<Option<MglValue<'ctx>>, Spanned<CodeGenError>> {
        match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => self
                .build_op_inner(op, lhs, rhs)
                .map(Some)
                .map_err(|e| Spanned::new(e, span)),
            _ => Ok(None),
        }
    }

    fn build_op_inner(
        &self,
        op: Op,
        lhs: MglValue<'ctx>,
        rhs: MglValue<'ctx>,
    ) -> Result<MglValue<'ctx>, CodeGenError> {
        if lhs.type_ != rhs.type_ {
            return Err(CodeGenError::InvalidOperandTypes {
                op: op.clone(),
                lhs: lhs.type_,
                rhs: rhs.type_,
            });
        }

        match lhs.type_ {
            MglType::Double => {
                let lhs = lhs.value.unwrap().into_float_value();
                let rhs = rhs.value.unwrap().into_float_value();

                match op {
                    Op::Lt => Ok(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::ULT, lhs, rhs, "lttmp")
                                .into(),
                        ),
                    }),
                    Op::Gt => Ok(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UGT, lhs, rhs, "gttmp")
                                .into(),
                        ),
                    }),
                    Op::Leq => Ok(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::ULE, lhs, rhs, "letmp")
                                .into(),
                        ),
                    }),
                    Op::Geq => Ok(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UGE, lhs, rhs, "getmp")
                                .into(),
                        ),
                    }),
                    Op::Eq => Ok(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UEQ, lhs, rhs, "eqtmp")
                                .into(),
                        ),
                    }),
                    Op::Neq => Ok(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UNE, lhs, rhs, "netmp")
                                .into(),
                        ),
                    }),
                    Op::Add => Ok(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_add(lhs, rhs, "addtmp").into()),
                    }),
                    Op::Sub => Ok(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_sub(lhs, rhs, "subtmp").into()),
                    }),
                    Op::Mul => Ok(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_mul(lhs, rhs, "multmp").into()),
                    }),
                    Op::Div => Ok(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_div(lhs, rhs, "divtmp").into()),
                    }),
                }
            }
            _ => Err(CodeGenError::InvalidOperandTypes {
                op,
                lhs: lhs.type_,
                rhs: rhs.type_,
            }),
        }
    }
}
