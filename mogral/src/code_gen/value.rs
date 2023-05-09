use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};

use crate::MglType;

/// 値か場所を表す式
#[derive(Clone)]
pub struct Expr<'ctx> {
    pub type_: MglType,
    pub category: ExprCategory<'ctx>,
}

#[derive(Clone)]
pub enum ExprCategory<'ctx> {
    Value(Option<BasicValueEnum<'ctx>>),
    Place(Option<PointerValue<'ctx>>),
}

/// 値を表す式 (value expression、右辺値)
#[derive(Clone)]
pub struct ValueExpr<'ctx> {
    pub type_: MglType,
    pub value: Option<BasicValueEnum<'ctx>>,
}

impl<'ctx> ValueExpr<'ctx> {
    pub fn unit() -> Self {
        Self {
            type_: MglType::Unit,
            value: None,
        }
    }
}

impl<'ctx> From<ValueExpr<'ctx>> for Expr<'ctx> {
    fn from(expr: ValueExpr<'ctx>) -> Self {
        Self {
            type_: expr.type_,
            category: ExprCategory::Value(expr.value),
        }
    }
}

/// 場所を表す式 (place expression、左辺値)
///
/// 変数などにも使用する
#[derive(Clone)]
pub struct PlaceExpr<'ctx> {
    pub type_: MglType,
    pub alloca: Option<PointerValue<'ctx>>,
}

impl<'ctx> From<PlaceExpr<'ctx>> for Expr<'ctx> {
    fn from(value: PlaceExpr<'ctx>) -> Self {
        Self {
            type_: value.type_,
            category: ExprCategory::Place(value.alloca),
        }
    }
}

impl<'ctx> TryFrom<Expr<'ctx>> for PlaceExpr<'ctx> {
    type Error = MglType;
    fn try_from(value: Expr<'ctx>) -> Result<Self, Self::Error> {
        match value.category {
            ExprCategory::Place(alloca) => Ok(Self {
                type_: value.type_,
                alloca,
            }),
            ExprCategory::Value(_) => Err(value.type_),
        }
    }
}

/// 値が存在しない可能性がある `Expr`
///
/// return式などでは制御を返さずに終了するため、
/// 値が存在しない可能性がある
#[must_use]
#[derive(Clone)]
pub enum MaybeNever<'ctx> {
    Value(Expr<'ctx>),
    Never,
}

impl<'ctx> From<Expr<'ctx>> for MaybeNever<'ctx> {
    fn from(expr: Expr<'ctx>) -> Self {
        Self::Value(expr)
    }
}

impl<'ctx> From<ValueExpr<'ctx>> for MaybeNever<'ctx> {
    fn from(expr: ValueExpr<'ctx>) -> Self {
        Self::Value(expr.into())
    }
}

impl<'ctx> From<PlaceExpr<'ctx>> for MaybeNever<'ctx> {
    fn from(expr: PlaceExpr<'ctx>) -> Self {
        Self::Value(expr.into())
    }
}

/// MograLで使用する関数
#[derive(Clone)]
pub struct MglFunction<'ctx> {
    pub params: Vec<MglType>,
    pub ret_type: MglType,
    pub value: FunctionValue<'ctx>,
}
