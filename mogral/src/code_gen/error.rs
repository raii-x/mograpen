use thiserror::Error;

use crate::op::{BinOp, UnOp};
use crate::types::MglType;

#[derive(Debug, PartialEq, Eq, Error)]
pub enum CodeGenError {
    #[error("invalid generated function")]
    InvalidFunction,
    #[error("invalid call produced")]
    InvalidCall,
    #[error("unresolved name: `{0}`")]
    UnresolvedName(String),
    #[error("variable already exists: `{0}`")]
    VariableAlreadyExists(String),
    #[error("multiple definitions of `{0}`")]
    MultipleDefinitions(String),
    #[error("invalid number of arguments passed (expected {expected}, found {found})")]
    InvalidNumberOfArguments { expected: usize, found: usize },
    #[error("mismatched types (expected {expected}, found {found})")]
    MismatchedTypes { expected: MglType, found: MglType },
    #[error("invalid binary operand types ({lhs} {op} {rhs})")]
    InvalidBinaryOperandTypes {
        op: BinOp,
        lhs: MglType,
        rhs: MglType,
    },
    #[error("invalid unary operand type ({op} {type_})")]
    InvalidUnaryOperandType { op: UnOp, type_: MglType },
    #[error("can't index non-indexable type: `{0}`")]
    NonIndexableType(MglType),
    #[error("invalid index type: `{0}`")]
    InvalidIndexType(MglType),
    #[error("assignment to non-place expression")]
    AssignmentToNonPlaceExpression,
}
