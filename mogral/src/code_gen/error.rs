use thiserror::Error;

use crate::op::Op;
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
    #[error("invalid number of arguments passed (expected {expected}, found {found})")]
    InvalidNumberOfArguments { expected: u32, found: u32 },
    #[error("mismatched types (expected {expected}, found {found})")]
    MismatchedTypes { expected: MglType, found: MglType },
    #[error("invalid operand types ({lhs} {op} {rhs})")]
    InvalidOperandTypes { op: Op, lhs: MglType, rhs: MglType },
}
