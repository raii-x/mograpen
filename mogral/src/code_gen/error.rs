use std::fmt::Display;

use thiserror::Error;

use crate::{op::Op, span::Spanned};

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

impl Display for Spanned<CodeGenError> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}-{}: {}", self.span.l, self.span.r, self.item)
    }
}
impl std::error::Error for Spanned<CodeGenError> {}
