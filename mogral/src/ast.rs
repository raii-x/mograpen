use crate::{op::Op, pos::Spanned};

#[derive(Debug)]
pub struct Module(pub Vec<Spanned<Func>>);

#[derive(Debug)]
pub enum Func {
    Extern(FuncDecl),
    FuncDef(FuncDef),
}

#[derive(Debug)]
pub struct FuncDecl {
    pub name: Spanned<String>,
    pub params: Vec<Spanned<String>>,
}

#[derive(Debug)]
pub struct FuncDef {
    pub decl: Spanned<FuncDecl>,
    pub body: Spanned<Block>,
}

#[derive(Debug)]
pub enum Stmt {
    Assign(Spanned<Assign>),
    Expr(Spanned<Box<Expr>>),
}

#[derive(Debug)]
pub struct Assign {
    pub var_name: Spanned<String>,
    pub val: Spanned<Box<Expr>>,
}

#[derive(Debug)]
pub enum Expr {
    Set(Assign),
    Return(Box<Expr>),
    Op(OpExpr),
    Number(f64),
    Ident(String),
    FuncCall(FuncCall),
    Block(Block),
    If(If),
    For(For),
}

#[derive(Debug)]
pub struct OpExpr {
    pub lhs: Spanned<Box<Expr>>,
    pub op: Spanned<Op>,
    pub rhs: Spanned<Box<Expr>>,
}

#[derive(Debug)]
pub struct FuncCall {
    pub func_name: Spanned<String>,
    pub args: Vec<Spanned<Box<Expr>>>,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Spanned<Stmt>>,
    pub expr: Option<Spanned<Box<Expr>>>,
}

#[derive(Debug)]
pub struct If {
    pub cond: Spanned<Box<Expr>>,
    pub then: Spanned<Block>,
    pub else_: Option<Spanned<Block>>,
}

#[derive(Debug)]
pub struct For {
    pub var_name: Spanned<String>,
    pub until: Spanned<Box<Expr>>,
    pub body: Spanned<Block>,
}
