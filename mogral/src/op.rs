use std::fmt::Display;

use strum_macros::IntoStaticStr;

#[derive(Debug, PartialEq, Eq, Clone, Copy, IntoStaticStr)]
pub enum BinOp {
    #[strum(serialize = "<")]
    Lt,
    #[strum(serialize = ">")]
    Gt,
    #[strum(serialize = "<=")]
    Leq,
    #[strum(serialize = ">=")]
    Geq,
    #[strum(serialize = "==")]
    Eq,
    #[strum(serialize = "!=")]
    Neq,
    #[strum(serialize = "+")]
    Add,
    #[strum(serialize = "-")]
    Sub,
    #[strum(serialize = "*")]
    Mul,
    #[strum(serialize = "/")]
    Div,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: &str = self.into();
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, IntoStaticStr)]
pub enum UnOp {
    #[strum(serialize = "-")]
    Neg,
    #[strum(serialize = "!")]
    Not,
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: &str = self.into();
        write!(f, "{}", s)
    }
}
