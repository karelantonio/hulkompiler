//! The expression types
//! See: [`Expr`] for a type containing all the possible expressions

use crate::Loc;

/// A binary operator
#[derive(Debug)]
pub enum BinOp {
    Cat,

    // Arithmetic
    Add,
    Sub,
    Mult,
    Div,
    Pwr,

    // Comparison
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,

    // Logic
    And,
    Or,
}

/// Add two values
#[derive(Debug)]
pub struct BinOpExpr {
    pub loc: Loc,
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

/// An unary operator
#[derive(Debug)]
pub enum UnaryOp {
    Neg, // -2
    Not, // !true
}

/// An unary operator expression
#[derive(Debug)]
pub struct UnaryOpExpr {
    pub loc: Loc,
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

/// A function call expression
#[derive(Debug)]
pub struct FunCallExpr {
    pub loc: Loc,
    pub name: String,
    pub args: Vec<Expr>,
}

/// A block, a sequence of expressions between { }
#[derive(Debug)]
pub struct BlockExpr {
    pub loc: Loc,
    pub exprs: Vec<Expr>,
}

/// A Variable declaration
#[derive(Debug)]
pub struct VarDeclExpr {
    pub loc: Loc,
    pub name: String,
    pub declty: Option<String>,
    pub expr: Box<Expr>,
    pub scope: Box<Expr>,
}

// A conditional expression
#[derive(Debug)]
pub struct IfExpr {
    pub loc: Loc,
    pub cond: Box<Expr>,
    pub ifarm: Box<Expr>,
    pub elsearm: Box<Expr>,
}

/// A while expression
#[derive(Debug)]
pub struct WhileExpr {
    pub loc: Loc,
    pub cond: Box<Expr>,
    pub body: Box<Expr>,
}

/// A reassign (destructive assignment)
#[derive(Debug)]
pub struct Reassign {
    pub loc: Loc,
    pub name: String,
    pub expr: Box<Expr>,
}

// An expression
#[derive(Debug)]
pub enum Expr {
    Num(Loc, f64),
    Id(Loc, String),
    Str(Loc, String),
    Boolean(Loc, bool),
    BinOpExpr(BinOpExpr),
    UnaryOpExpr(UnaryOpExpr),
    FunCallExpr(FunCallExpr),
    BlockExpr(BlockExpr),
    VarDeclExpr(VarDeclExpr),
    IfExpr(IfExpr),
    WhileExpr(WhileExpr),
    Reassign(Reassign),
}

impl Expr {
    pub fn loc(&self) -> &Loc {
        match self {
            Expr::Num(loc, _) => loc,
            Expr::Id(loc, _) => loc,
            Expr::Str(loc, _) => loc,
            Expr::Boolean(loc, _) => loc,
            Expr::BinOpExpr(BinOpExpr { loc, .. }) => loc,
            Expr::FunCallExpr(FunCallExpr { loc, .. }) => loc,
            Expr::BlockExpr(BlockExpr { loc, .. }) => loc,
            Expr::UnaryOpExpr(UnaryOpExpr { loc, .. }) => loc,
            Expr::VarDeclExpr(VarDeclExpr { loc, .. }) => loc,
            Expr::IfExpr(IfExpr { loc, .. }) => loc,
            Expr::WhileExpr(WhileExpr { loc, .. }) => loc,
            Expr::Reassign(Reassign { loc, .. }) => loc,
        }
    }
}
