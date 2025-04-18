//! Expression types, see: [`Expr`]

use crate::{
    ops::{Op, UOp},
    ty::Ty,
};


/// A constant id (for references)
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct ConstId(pub(crate)usize);

/// The value of a constant (may be inside a constant pool)
#[derive(Debug)]
pub enum ConstValue {
    Num(f64),
    Str(String),
    Bool(bool),
}

/// The value of a constant
#[derive(Debug)]
pub struct Const {
    pub id: ConstId,
    pub value: ConstValue,
}

/// A function ID (for references)
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct FunId(pub(crate)usize);

/// A variable ID
#[derive(Debug, Clone, Copy)]
pub struct VarId(pub(crate)usize);

impl VarId {
    pub fn id(&self) -> usize {
        self.0
    }
}

/// A variable kind
#[derive(Debug, Clone)]
pub enum VarKind {
    Local,
    Param,
    Global,
}

/// A variable
#[derive(Debug, Clone)]
pub struct Var {
    pub id: VarId,
    pub kind: VarKind,
    pub name: String,
    pub ty: Ty,
}

/// A function body
#[derive(Debug)]
pub enum FunBody {
    Expr(Expr),
    Std,
    Native, // To make things more interesting :)
}

/// A function argument
#[derive(Debug, Clone)]
pub struct FunArg {
    pub name: String,
    pub ty: Ty,
    pub id: VarId,
}

/// A function
#[derive(Debug)]
pub struct Fun {
    pub id: FunId,
    pub name: String,
    pub ty: Ty,
    pub args: Vec<FunArg>,
    pub body: FunBody,
}

/// An expression
#[derive(Debug)]
pub enum Expr {
    Const {
        cons: ConstId,
        ty: Ty,
    },
    BinOp {
        op: Op,
        ty: Ty,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Call {
        fun: FunId,
        ty: Ty,
        args: Vec<Expr>,
    },
    Block {
        ty: Ty,
        insts: Vec<Expr>,
    },
    ImplicitCast {
        ty: Ty,
        expr: Box<Expr>,
    },
    VarRead {
        ty: Ty,
        var: VarId,
    },
    VarDecl {
        ty: Ty,
        var: VarId,
        expr: Box<Expr>,
        scope: Box<Expr>,
    },
    UnaryOp {
        ty: Ty,
        op: UOp,
        expr: Box<Expr>,
    },
    Branch {
        ty: Ty,
        cond: Box<Expr>,
        ontrue: Box<Expr>,
        onfalse: Box<Expr>,
    },
    Loop {
        ty: Ty,
        cond: Box<Expr>,
        body: Box<Expr>,
    },
    Reassign {
        ty: Ty,
        var: VarId,
        expr: Box<Expr>,
    },
}

impl Expr {
    pub fn ty(&self) -> Ty {
        *match self {
            Self::Const { ty, .. } => ty,
            Self::BinOp { ty, .. } => ty,
            Self::Call { ty, .. } => ty,
            Self::Block { ty, .. } => ty,
            Self::ImplicitCast { ty, .. } => ty,
            Self::VarRead { ty, .. } => ty,
            Self::UnaryOp { ty, .. } => ty,
            Self::VarDecl { ty, .. } => ty,
            Self::Branch { ty, .. } => ty,
            Self::Loop { ty, .. } => ty,
            Self::Reassign { ty, .. } => ty,
        }
    }

    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const { .. })
    }
}
