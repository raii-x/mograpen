use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

impl Op {
    pub fn as_str(self) -> &'static str {
        match self {
            Op::Lt => "<",
            Op::Gt => ">",
            Op::Leq => "<=",
            Op::Geq => ">=",
            Op::Eq => "==",
            Op::Neq => "!=",
            Op::Add => "+",
            Op::Sub => "-",
            Op::Mul => "*",
            Op::Div => "/",
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
