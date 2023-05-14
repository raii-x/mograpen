use std::fmt::Display;

use strum_macros::IntoStaticStr;

/// MograLで使用する型
#[derive(Debug, PartialEq, Eq, Clone, IntoStaticStr)]
pub enum MglType {
    #[strum(serialize = "()")]
    Unit,
    #[strum(serialize = "bool")]
    Bool,
    #[strum(serialize = "int")]
    Int,
    #[strum(serialize = "double")]
    Double,
    #[strum(serialize = "array")]
    Array { type_: Box<MglType>, size: usize },
}

impl Display for MglType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: &str = self.into();
        write!(f, "{}", s)
    }
}
