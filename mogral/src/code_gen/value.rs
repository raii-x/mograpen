use inkwell::{
    builder::Builder,
    context::Context,
    types::{BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue, InstructionValue, PointerValue},
    FloatPredicate, IntPredicate,
};

use crate::{
    op::Op,
    pos::{Span, Spanned},
    types::MglType,
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
                let lhs_v = lhs.value.unwrap().into_float_value();
                let rhs_v = rhs.value.unwrap().into_float_value();

                match op {
                    Op::Lt => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::ULT, lhs_v, rhs_v, "lttmp")
                                .into(),
                        ),
                    }),
                    Op::Gt => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UGT, lhs_v, rhs_v, "gttmp")
                                .into(),
                        ),
                    }),
                    Op::Leq => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::ULE, lhs_v, rhs_v, "letmp")
                                .into(),
                        ),
                    }),
                    Op::Geq => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UGE, lhs_v, rhs_v, "getmp")
                                .into(),
                        ),
                    }),
                    Op::Eq => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UEQ, lhs_v, rhs_v, "eqtmp")
                                .into(),
                        ),
                    }),
                    Op::Neq => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_float_compare(FloatPredicate::UNE, lhs_v, rhs_v, "netmp")
                                .into(),
                        ),
                    }),
                    Op::Add => Some(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_add(lhs_v, rhs_v, "addtmp").into()),
                    }),
                    Op::Sub => Some(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_sub(lhs_v, rhs_v, "subtmp").into()),
                    }),
                    Op::Mul => Some(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_mul(lhs_v, rhs_v, "multmp").into()),
                    }),
                    Op::Div => Some(MglValue {
                        type_: MglType::Double,
                        value: Some(self.builder.build_float_div(lhs_v, rhs_v, "divtmp").into()),
                    }),
                }
            }
            MglType::Bool => {
                let lhs_v = lhs.value.unwrap().into_int_value();
                let rhs_v = rhs.value.unwrap().into_int_value();

                match op {
                    Op::Eq => Some(MglValue {
                        type_: MglType::Bool,
                        value: Some(
                            self.builder
                                .build_int_compare(IntPredicate::EQ, lhs_v, rhs_v, "eqtmp")
                                .into(),
                        ),
                    }),
                    Op::Neq => Some(MglValue {
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
                Op::Eq => Some(self.bool(true)),
                Op::Neq => Some(self.bool(false)),
                _ => None,
            },
        }
        .ok_or(CodeGenError::InvalidOperandTypes {
            op,
            lhs: lhs.type_,
            rhs: rhs.type_,
        })
    }
}

#[derive(Clone)]
pub struct MglFunction<'ctx> {
    pub params: Vec<MglType>,
    pub ret_type: MglType,
    pub value: FunctionValue<'ctx>,
}
