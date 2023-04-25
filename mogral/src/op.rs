use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Lt,
    Gt,
    Leq,
    Geq,
    Eq,
    Neq,
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Lt => write!(f, "<"),
            Op::Gt => write!(f, ">"),
            Op::Leq => write!(f, "<="),
            Op::Geq => write!(f, ">="),
            Op::Eq => write!(f, "=="),
            Op::Neq => write!(f, "!="),
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
        }
    }
}
