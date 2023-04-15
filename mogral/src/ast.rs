#[derive(Debug)]
pub struct Module(pub Vec<Func>);

#[derive(Debug)]
pub enum Func {
    Extern(FuncDecl),
    FuncDef(FuncDef),
}

#[derive(Debug)]
pub struct FuncDecl {
    pub name: String,
    pub params: Vec<String>,
}

#[derive(Debug)]
pub struct FuncDef {
    pub decl: FuncDecl,
    pub body: Block,
}

#[derive(Debug)]
pub enum Stmt {
    Assign(Assign),
    Expr(Box<Expr>),
}

#[derive(Debug)]
pub struct Assign {
    pub var_name: String,
    pub val: Box<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Set(Assign),
    Return(Box<Expr>),
    Op(Box<Expr>, Op, Box<Expr>),
    Number(f64),
    Ident(String),
    FuncCall(FuncCall),
    Block(Block),
    If(If),
    For(For),
}

#[derive(Debug)]
pub struct FuncCall {
    pub func_name: String,
    pub args: Vec<Box<Expr>>,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug)]
pub struct If {
    pub cond: Box<Expr>,
    pub then: Block,
    pub else_: Option<Block>,
}

#[derive(Debug)]
pub struct For {
    pub var_name: String,
    pub until: Box<Expr>,
    pub body: Block,
}

#[derive(Debug)]
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
