use std::fmt::Display;

use strum_macros::IntoStaticStr;

/// MograLで使用する型
#[derive(Debug, PartialEq, Eq, Clone, Copy, IntoStaticStr)]
pub enum MglType {
    #[strum(serialize = "()")]
    Unit,
    #[strum(serialize = "double")]
    Double,
    #[strum(serialize = "bool")]
    Bool,
}

impl Display for MglType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: &str = self.into();
        write!(f, "{}", s)
    }
}
