use crate::{op::Op, pos::Spanned};

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
    params: Vec<Spanned<String>>
}

ast_node_struct! {
    FuncDef;
    decl: Spanned<FuncDecl>,
    body: Spanned<Block>
}

ast_node_enum! {
    Stmt;
    Assign(Spanned<Assign>),
    Expr(Spanned<Box<Expr>>)
}

ast_node_struct! {
    Assign;
    var_name: Spanned<String>,
    val: Spanned<Box<Expr>>
}

ast_node_enum! {
    Expr;
    Set(Assign),
    Return(Box<Expr>),
    Op(OpExpr),
    Number(f64),
    Ident(String),
    FuncCall(FuncCall),
    Block(Block),
    If(If),
    For(For)
}

ast_node_struct! {
    OpExpr;
    lhs: Spanned<Box<Expr>>,
    op: Spanned<Op>,
    rhs: Spanned<Box<Expr>>
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

impl ASTNode for String {
    fn node_to_string(&self) -> String {
        "\"".to_owned() + self + "\""
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

impl ASTNode for Op {
    fn node_to_string(&self) -> String {
        "\"".to_owned() + self.as_str() + "\""
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
