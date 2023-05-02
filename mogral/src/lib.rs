#[macro_use]
extern crate lalrpop_util;

pub mod ast;
mod code_gen;
mod exec;
mod op;
mod parser;
mod pos;
mod types;
mod wrapper;

pub use code_gen::code_gen;
pub use code_gen::error::CodeGenError;
pub use exec::exec;
pub use op::Op;
pub use parser::error::parse_error_pos;
pub use parser::parse;
pub use pos::{SourcePosConverter, Span, Spanned};
pub use types::MglType;
pub use wrapper::{MglContext, MglModule};
