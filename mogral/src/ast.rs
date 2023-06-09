use crate::{pos::Spanned, BinOp, LazyBinOp, MglType, UnOp};

/// ASTのノードをツリーとして表示するためのトレイト
pub trait ASTNode {
    /// ASTノード自身を表す文字列を取得
    fn node_to_string(&self) -> String;
    /// ノードの子をVecで返す
    fn children(&self) -> Vec<(String, &dyn ASTNode)>;
}

/// ASTNodeトレイトを実装した構造体を定義する
///
/// 構造体のフィールドは全てpubとなる
macro_rules! ast_node_struct {
    { $name:ident; $($i:ident : $t:ty),* } => {
        #[derive(Debug)]
        pub struct $name {
            $(pub $i: $t),*
        }

        impl ASTNode for $name {
            fn node_to_string(&self) -> String {
                stringify!($name).to_owned()
            }
            fn children(&self) -> Vec<(String, &dyn ASTNode)> {
                vec![$((stringify!($i).to_owned(), &self.$i)),*]
            }
        }
    };
}

/// ASTNodeトレイトを実装したenumを定義する
macro_rules! ast_node_enum {
    { $name:ident; $($i:ident($t:ty)),* } => {
        #[derive(Debug)]
        pub enum $name {
            $($i($t)),*
        }

        impl ASTNode for $name {
            fn node_to_string(&self) -> String {
                match self {
                    $(Self::$i(x) => x.node_to_string()),*
                }
            }
            fn children(&self) -> Vec<(String, &dyn ASTNode)> {
                match self {
                    $(Self::$i(x) => x.children()),*
                }
            }
        }
    };
}

ast_node_struct! {
    Module;
    funcs: Vec<Spanned<Func>>
}

ast_node_enum! {
    Func;
    Extern(FuncDecl),
    FuncDef(FuncDef)
}

ast_node_struct! {
    FuncDecl;
    name: Spanned<String>,
    params: Vec<Spanned<TypedIdent>>,
    ret: Option<Spanned<MglType>>
}

ast_node_struct! {
    FuncDef;
    decl: Spanned<FuncDecl>,
    body: Spanned<Block>
}

ast_node_enum! {
    Stmt;
    Let(Spanned<Let>),
    Expr(Spanned<Box<Expr>>)
}

ast_node_struct! {
    Let;
    name: Spanned<String>,
    type_: Option<Spanned<MglType>>,
    val: Option<Spanned<Box<Expr>>>
}

ast_node_enum! {
    Expr;
    Set(Set),
    Return(Box<Expr>),
    BinOp(BinOpExpr),
    LazyBinOp(LazyBinOpExpr),
    UnOp(UnOpExpr),
    Index(Index),
    Literal(Literal),
    Ident(String),
    FuncCall(FuncCall),
    Block(Block),
    If(If),
    For(For)
}

ast_node_struct! {
    Set;
    var: Spanned<Box<Expr>>,
    val: Spanned<Box<Expr>>
}

ast_node_struct! {
    BinOpExpr;
    lhs: Spanned<Box<Expr>>,
    op: Spanned<BinOp>,
    rhs: Spanned<Box<Expr>>
}

ast_node_struct! {
    LazyBinOpExpr;
    lhs: Spanned<Box<Expr>>,
    op: Spanned<LazyBinOp>,
    rhs: Spanned<Box<Expr>>
}

ast_node_struct! {
    UnOpExpr;
    op: Spanned<UnOp>,
    opnd: Spanned<Box<Expr>>
}

ast_node_struct! {
    Index;
    target: Spanned<Box<Expr>>,
    index: Spanned<Box<Expr>>
}

ast_node_enum! {
    Literal;
    // unit型は0要素のタプルとすべきだが、暫定的に()をリテラルとしている
    Unit(()),
    Bool(bool),
    Int(i32),
    Float(f64)
}

ast_node_struct! {
    FuncCall;
    func_name: Spanned<String>,
    args: Vec<Spanned<Box<Expr>>>
}

ast_node_struct! {
    Block;
    stmts: Vec<Spanned<Stmt>>,
    expr: Option<Spanned<Box<Expr>>>
}

ast_node_struct! {
    If;
    cond: Spanned<Box<Expr>>,
    then: Spanned<Block>,
    else_: Option<Spanned<Block>>
}

ast_node_struct! {
    For;
    var_name: Spanned<String>,
    until: Spanned<Box<Expr>>,
    body: Spanned<Block>
}

ast_node_struct! {
    TypedIdent;
    ident: Spanned<String>,
    type_: Spanned<MglType>
}

impl ASTNode for String {
    fn node_to_string(&self) -> String {
        "\"".to_owned() + self + "\""
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        vec![]
    }
}

impl ASTNode for () {
    fn node_to_string(&self) -> String {
        "()".to_owned()
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        vec![]
    }
}

impl ASTNode for bool {
    fn node_to_string(&self) -> String {
        self.to_string()
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        vec![]
    }
}

impl ASTNode for i32 {
    fn node_to_string(&self) -> String {
        self.to_string()
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        vec![]
    }
}

impl ASTNode for f64 {
    fn node_to_string(&self) -> String {
        self.to_string()
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        vec![]
    }
}

impl ASTNode for BinOp {
    fn node_to_string(&self) -> String {
        "\"".to_owned() + self.into() + "\""
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        vec![]
    }
}

impl ASTNode for LazyBinOp {
    fn node_to_string(&self) -> String {
        "\"".to_owned() + self.into() + "\""
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        vec![]
    }
}

impl ASTNode for UnOp {
    fn node_to_string(&self) -> String {
        "\"".to_owned() + self.into() + "\""
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        vec![]
    }
}

impl ASTNode for MglType {
    fn node_to_string(&self) -> String {
        "\"".to_owned() + self.into() + "\""
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        vec![]
    }
}

impl<T: ASTNode> ASTNode for Spanned<T> {
    fn node_to_string(&self) -> String {
        self.item.node_to_string()
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        self.item.children()
    }
}

impl<T: ASTNode> ASTNode for Vec<T> {
    fn node_to_string(&self) -> String {
        "Vec".to_owned()
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        self.iter()
            .enumerate()
            .map(|(i, x)| (i.to_string(), x as &dyn ASTNode))
            .collect()
    }
}

impl<T: ASTNode> ASTNode for Box<T> {
    fn node_to_string(&self) -> String {
        self.as_ref().node_to_string()
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        self.as_ref().children()
    }
}

impl<T: ASTNode> ASTNode for Option<T> {
    fn node_to_string(&self) -> String {
        match self {
            Some(x) => x.node_to_string(),
            None => "None".to_owned(),
        }
    }
    fn children(&self) -> Vec<(String, &dyn ASTNode)> {
        match self {
            Some(x) => x.children(),
            None => vec![],
        }
    }
}
