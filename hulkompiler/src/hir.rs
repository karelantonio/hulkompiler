//! The high-level intermediate representation of the language
//! Makes easy the type-checking phase :)

use thiserror::Error;

/// A constant id (for references)
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct ConstId(usize);

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

/// A type (may be inside a type pool)
#[derive(Clone, Copy, Debug)]
pub enum Ty {
    Num,
    Str,
    Bool,
}

/// A binary operation
#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Cat,
}

impl Op {
    fn is_arithmetic(&self) -> bool {
        match self {
            Self::Add | Self::Sub | Self::Mul | Self::Div | Self::Pow => true,
            _ => false,
        }
    }
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
}

impl Expr {
    pub fn ty(&self) -> Ty {
        match self {
            Self::Const { ty, .. } => ty,
            Self::BinOp { ty, .. } => ty,
            Self::Call { ty, .. } => ty,
        }
        .clone()
    }
}

/// A function ID (for references)
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct FunId(usize);

/// A function
pub struct Fun {
    pub id: FunId,
    pub name: String,
    pub ty: Ty,
    pub body: Expr,
}

#[derive(Debug)]
pub struct Unit {
    pub constpool: Vec<Const>,
    pub expr: Expr,
}

/// Errors that may happend during the type checking phase
#[derive(Debug, Error)]
pub enum TypeError {
    #[error("Unknown reference to function: {name}")]
    UnknownFun { name: String },

    #[error("Unknown reference to type: {name}")]
    UnknownTy { name: String },

    #[error("Unknown reference to variable: {name}")]
    UnknownVar { name: String },

    #[error("Both expressions must be numbers in order to apply an arithmetic operator")]
    ExprMustBeNumbers,
}

type TResult<T> = Result<T, TypeError>;

/// This is the type checker, this transforms the data structure into a high-level representation
/// of the code
#[derive(Default)]
pub struct TypeChecker {
    constpool: Vec<Const>,
}

impl TypeChecker {
    /// Store the given const num
    fn push_const_num(&mut self, val: f64) -> ConstId {
        let id = self.constpool.len();
        self.constpool.push(Const {
            id: ConstId(id),
            value: ConstValue::Num(val),
        });

        ConstId(id)
    }

    /// Store the given const str
    fn push_const_str(&mut self, val: &str) -> ConstId {
        let id = self.constpool.len();
        self.constpool.push(Const {
            id: ConstId(id),
            value: ConstValue::Str(val.into()),
        });
        ConstId(id)
    }

    /// Transform the given expresion to the given unit
    pub fn transform(stm: &crate::ast::Expr) -> TResult<Unit> {
        // Transform this expr to our expr
        let mut inst = Self::default();
        let e = inst.to_expr(stm)?;
        Ok(Unit {
            constpool: inst.constpool,
            expr: e,
        })
    }

    fn to_op(&mut self, op: &crate::ast::BinOp) -> Op {
        match op {
            crate::ast::BinOp::Add => Op::Add,
            crate::ast::BinOp::Sub => Op::Sub,
            crate::ast::BinOp::Mult => Op::Mul,
            crate::ast::BinOp::Div => Op::Div,
            crate::ast::BinOp::Pwr => Op::Pow,
        }
    }

    fn to_expr(&mut self, expr: &crate::ast::Expr) -> TResult<Expr> {
        Ok(match expr {
            crate::ast::Expr::Num(v) => Expr::Const {
                ty: Ty::Num,
                cons: self.push_const_num(*v),
            },

            crate::ast::Expr::Str(v) => Expr::Const {
                ty: Ty::Str,
                cons: self.push_const_str(&v),
            },

            crate::ast::Expr::BinOpExpr(binop) => {
                let op = self.to_op(&binop.op);
                let left = self.to_expr(&binop.left)?;
                let right = self.to_expr(&binop.right)?;

                // If is arithmetic operator
                if op.is_arithmetic()
                    && !(matches!(left.ty(), Ty::Num) && matches!(right.ty(), Ty::Num))
                {
                    return Err(TypeError::ExprMustBeNumbers);
                }

                Expr::BinOp {
                    ty: Ty::Num,
                    op,
                    left: left.into(),
                    right: right.into(),
                }
            }

            e => {
                panic!("Unsupported expresion: {e:?}");
            }
        })
    }
}
