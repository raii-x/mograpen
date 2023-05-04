#[macro_use]
extern crate lalrpop_util;

pub mod ast;
mod code_gen;
mod op;
mod parser;
mod pos;
mod types;

pub use code_gen::code_gen;
pub use code_gen::error::CodeGenError;
pub use inkwell;
pub use op::Op;
pub use parser::error::parse_error_pos;
pub use parser::parse;
pub use pos::{SourcePosConverter, Span, Spanned};
pub use types::MglType;
