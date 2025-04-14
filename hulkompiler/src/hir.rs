//! The high-level intermediate representation of the language
//! Makes easy the type-checking phase :)

use std::collections::BTreeMap;
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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    Void, /* Nothing */
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

    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const { .. })
    }
}

/// A function ID (for references)
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct FunId(usize);

/// A function body
#[derive(Debug)]
pub enum FunBody {
    Expr(Expr),
    Std,
    Native, // To make things more interesting :)
}

/// A function
#[derive(Debug)]
pub struct Fun {
    pub id: FunId,
    pub name: String,
    pub ty: Ty,
    pub args: Vec<Ty>,
    pub body: FunBody,
}

#[derive(Debug)]
pub struct Unit {
    pub funpool: Vec<Fun>,
    pub constpool: Vec<Const>,
    pub expr: Expr,
}

impl Unit {
    pub fn lookup_const(&self, ConstId(cid): &ConstId) -> Option<&Const> {
        self.constpool.get(*cid)
    }

    pub fn lookup_fun(&self, FunId(fid): &FunId) -> Option<&Fun> {
        self.funpool.get(*fid)
    }
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

    #[error("Required {req} args for function {fname} but {found} were provided")]
    InvalidNumberOfArgs {
        req: usize,
        fname: String,
        found: usize,
    },

    #[error("{idx}th argument type mismatch, expected: {exp:?}, found: {found:?}")]
    FnArgMismatch { idx: usize, exp: Ty, found: Ty },
}

type TResult<T> = Result<T, TypeError>;

/// This is the type checker, this transforms the data structure into a high-level representation
/// of the code
#[derive(Default)]
pub struct TypeChecker {
    constpool: Vec<Const>,
    funpool: Vec<Fun>,
    reverse_fun: BTreeMap<String, FunId>,
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

    fn add_std_fun(&mut self, name: &str, ret: Ty, args: &[Ty]) {
        let newid = self.funpool.len();
        let fun = Fun {
            id: FunId(newid),
            name: name.into(),
            ty: ret,
            body: FunBody::Std,
            args: args.into(),
        };
        self.funpool.push(fun);
        self.reverse_fun.insert(name.into(), FunId(newid));
    }

    /// Transform the given expresion to the given unit
    pub fn transform(stm: &crate::ast::Expr) -> TResult<Unit> {
        // Transform this expr to our expr
        let mut inst = Self::default();

        // Add the Standard library functions
        inst.add_std_fun("print", Ty::Void, &[Ty::Str]);
        // -- math functions --
        inst.add_std_fun("sqrt", Ty::Num, &[Ty::Num]);
        inst.add_std_fun("sin", Ty::Num, &[Ty::Num]);
        inst.add_std_fun("cos", Ty::Num, &[Ty::Num]);
        inst.add_std_fun("exp", Ty::Num, &[Ty::Num]);
        inst.add_std_fun("log", Ty::Num, &[Ty::Num, Ty::Num]);
        inst.add_std_fun("rand", Ty::Num, &[]);

        let e = inst.to_expr(stm)?;
        Ok(Unit {
            constpool: inst.constpool,
            expr: e,
            funpool: inst.funpool,
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

            crate::ast::Expr::FunCallExpr(fun) => {
                // Check if the function exists
                let FunId(funid) = self
                    .reverse_fun
                    .get(&fun.name)
                    .ok_or(TypeError::UnknownFun {
                        name: fun.name.clone(),
                    })?
                    .clone();

                let ty = {
                    let funref = &self.funpool[funid];

                    if fun.args.len() != funref.args.len() {
                        return Err(TypeError::InvalidNumberOfArgs {
                            req: funref.args.len(),
                            fname: funref.name.clone(),
                            found: fun.args.len(),
                        });
                    }

                    funref.ty.clone()
                };

                // The arguments with its type checked
                let mut args = Vec::new();

                for (i, arg) in fun.args.iter().enumerate() {
                    // Check if args match
                    let expr = self.to_expr(arg)?;

                    let req = self.funpool[funid].args[i];
                    if expr.ty() != req {
                        // Argument type mismatch
                        return Err(TypeError::FnArgMismatch {
                            idx: i + 1,
                            exp: req.clone(),
                            found: expr.ty(),
                        });
                    }

                    args.push(expr);
                }

                Expr::Call {
                    fun: FunId(funid),
                    ty,
                    args,
                }
            }

            e => {
                panic!("Unsupported expresion: {e:?}");
            }
        })
    }
}
