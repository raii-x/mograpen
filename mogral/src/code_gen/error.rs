use thiserror::Error;

use crate::op::Op;

use super::value::MglType;

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("invalid generated function")]
    InvalidFunction,
    #[error("unknown variable name")]
    UnknownVariableName,
    #[error("variable already exists")]
    VariableAlreadyExists,
    #[error("unknown function referenced")]
    UnknownFunction,
    #[error("incorrect number of arguments passed")]
    IncorrectNumberOfArguments,
    #[error("invalid call produced")]
    InvalidCall,
    #[error("mismatched types (expected {expected:?}, found {found:?})")]
    MismatchedTypes { expected: MglType, found: MglType },
    #[error("invalid operand types ({lhs:?} {op} {rhs:?})")]
    InvalidOperandTypes { op: Op, lhs: MglType, rhs: MglType },
}
