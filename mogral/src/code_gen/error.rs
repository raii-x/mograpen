use thiserror::Error;

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
}
