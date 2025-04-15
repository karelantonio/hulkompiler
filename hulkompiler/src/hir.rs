//! The high-level intermediate representation of the language
//! Makes easy the type-checking phase :)

use std::collections::{BTreeMap, BTreeSet};
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
    Obj,
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
    Block {
        ty: Ty,
        insts: Vec<Expr>,
    },
    ImplicitCast {
        ty: Ty,
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
        }
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

/// A function argument
#[derive(Debug, Clone)]
pub struct FunArg {
    pub name: String,
    pub ty: Ty,
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

    #[error("{idx}th argument ({name}) type mismatch, the given type ({found:?}) can not be interpreted as {exp:?} (does not conform {exp:?})")]
    FnArgMismatch {
        idx: usize,
        name: String,
        exp: Ty,
        found: Ty,
    },

    #[error(
        "No global expression found, try `print(\"Hello world\");` at the bottom of your program"
    )]
    NoGlobExpr,

    #[error("Multiple global expressions were found, but only 1 is allowed, try removing one of those or group them using a code block '{{ ... }}' ")]
    MultipleGlobExpr,

    #[error("A function with the same name ({name}) is already defined, try another name or use a prefix")]
    DupdFun { name: String },

    #[error("Function arguments with no type are not supported yet, try specifying the type: `..{name}: Smthn`")]
    FnArgNoType { name: String },

    #[error("Function without return type are not supported yet: {name}, try specifying a type: `): Smthn`")]
    FnNoRetType { name: String },

    #[error("Function return mismatch, body type ({bod}) can not be interpreted as ({ret}) (does not conform {ret})")]
    FnRetMismatch { ret: String, bod: String },
}

type TResult<T> = Result<T, TypeError>;

/// This is the type checker, this transforms the data structure into a high-level representation
/// of the code
#[derive(Default)]
pub struct TypeChecker {
    constpool: Vec<Const>,
    funpool: Vec<Fun>,
    reverse_fun: BTreeMap<String, FunId>,
    stubd: BTreeSet<usize>,
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

    fn add_stub_fun(&mut self, name: &str, ret: Ty, args: &[FunArg], std: bool) -> usize {
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

        if !std {
            self.stubd.insert(newid);
        }

        newid
    }

    fn is_stub(&mut self, FunId(fid): &FunId) -> bool {
        self.stubd.contains(fid)
    }

    fn unstub(&mut self, FunId(fid): &FunId) {
        self.stubd.remove(fid);
    }

    fn update_fn_body(&mut self, FunId(fid): &FunId, body: FunBody) {
        self.funpool[*fid].body = body;
    }

    fn str_to_ty(&mut self, ty: &str) -> Option<Ty> {
        Some(if ty == "Number" {
            Ty::Num
        } else if ty == "String" {
            Ty::Str
        } else if ty == "Boolean" {
            Ty::Bool
        } else if ty == "Object" {
            Ty::Obj
        } else {
            return None;
        })
    }

    fn ty_conforms(&self, from: &Ty, to: &Ty) -> bool {
        match to {
            Ty::Obj => true,
            another => another == from,
        }
    }

    /// Transform the given expresion to the given unit
    pub fn transform(ast: &[crate::ast::RootElem]) -> TResult<Unit> {
        // Transform this expr to our expr
        let mut tr = Self::default();

        // Add the Standard library functions
        tr.add_stub_fun(
            "print",
            Ty::Obj,
            &[FunArg {
                name: "arg0".into(),
                ty: Ty::Obj,
            }],
            true,
        );
        // -- math functions --
        tr.add_stub_fun(
            "sqrt",
            Ty::Num,
            &[FunArg {
                name: "arg0".into(),
                ty: Ty::Num,
            }],
            true,
        );
        tr.add_stub_fun(
            "sin",
            Ty::Num,
            &[FunArg {
                name: "arg0".into(),
                ty: Ty::Num,
            }],
            true,
        );
        tr.add_stub_fun(
            "cos",
            Ty::Num,
            &[FunArg {
                name: "arg0".into(),
                ty: Ty::Num,
            }],
            true,
        );
        tr.add_stub_fun(
            "exp",
            Ty::Num,
            &[FunArg {
                name: "arg0".into(),
                ty: Ty::Num,
            }],
            true,
        );
        tr.add_stub_fun(
            "log",
            Ty::Num,
            &[
                FunArg {
                    name: "arg0".into(),
                    ty: Ty::Num,
                },
                FunArg {
                    name: "arg1".into(),
                    ty: Ty::Num,
                },
            ],
            true,
        );
        tr.add_stub_fun("rand", Ty::Num, &[], true);

        // First, add the stub functions
        tr.create_fun_stubs(ast)?;

        // Now create the functions
        for elem in ast {
            let crate::ast::RootElem::FunDecl(fun) = elem else {
                continue;
            };

            tr.process_fn(&fun)?;
        }

        // Now the actual global expression
        let mut glob = None;

        for elem in ast {
            let crate::ast::RootElem::Statement(expr) = elem else {
                continue;
            };

            if glob.is_some() {
                // Already found a glob expression
                return Err(TypeError::MultipleGlobExpr);
            }

            glob = Some(tr.to_expr(expr)?);
        }

        Ok(Unit {
            constpool: tr.constpool,
            expr: glob.ok_or(TypeError::NoGlobExpr)?,
            funpool: tr.funpool,
        })
    }

    fn create_fun_stubs(&mut self, ast: &[crate::ast::RootElem]) -> TResult<()> {
        for elem in ast {
            let crate::ast::RootElem::FunDecl(fun) = elem else {
                continue;
            };
            // The args
            let args = fun
                .args
                .iter()
                .map(|e| {
                    let aname = e.name.clone();
                    let tyname = e.ty.clone();

                    let argty = &tyname.ok_or(TypeError::FnArgNoType {
                        name: aname.clone(),
                    })?;
                    Ok(FunArg {
                        name: aname.clone(),
                        ty: self
                            .str_to_ty(&argty)
                            .ok_or(TypeError::UnknownTy { name: aname.into() })?,
                    })
                })
                .collect::<Result<Vec<_>, TypeError>>()?;

            let ret = fun.ret.clone().ok_or(TypeError::FnNoRetType {
                name: fun.name.clone(),
            })?;
            let ret = self
                .str_to_ty(&ret)
                .ok_or(TypeError::UnknownTy { name: ret.into() })?;

            self.add_stub_fun(&fun.name, ret, &args, false);
        }

        Ok(())
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

    fn process_fn(&mut self, fun: &crate::ast::FunDecl) -> TResult<()> {
        // Check is not already defined (no stub)
        let FunId(fid) = self.reverse_fun[&fun.name]; // Its ok to panic, because we actually EXPECT
                                                      // fun.name to be a stub

        if !self.is_stub(&FunId(fid)) {
            return Err(TypeError::DupdFun {
                name: fun.name.clone(),
            });
        }

        // Transform the function body
        let body = self.to_expr(&fun.body)?;

        // Add the function to the pool
        self.update_fn_body(&FunId(fid), FunBody::Expr(body));
        self.unstub(&FunId(fid));

        Ok(())
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
                    let mut expr = self.to_expr(arg)?;

                    let req = self.funpool[funid].args[i].clone();
                    if !self.ty_conforms(&expr.ty(), &req.ty) {
                        // Argument type mismatch
                        return Err(TypeError::FnArgMismatch {
                            idx: i + 1,
                            name: req.name,
                            exp: req.ty,
                            found: expr.ty(),
                        });
                    }
                    // We might introduce an implicit cast if they're different
                    if expr.ty() != req.ty {
                        // TODO: Check whether we are not doing two implicit casts, if so reduce to
                        // one
                        expr = Expr::ImplicitCast {
                            ty: req.ty,
                            expr: Box::new(expr),
                        };
                    }

                    args.push(expr);
                }

                Expr::Call {
                    fun: FunId(funid),
                    ty,
                    args,
                }
            }

            crate::ast::Expr::BlockExpr(crate::ast::BlockExpr { exprs }) => {
                // Map each of these
                let instrs = exprs
                    .iter()
                    .map(|e| self.to_expr(e))
                    .collect::<Result<Vec<_>, TypeError>>()?;

                Expr::Block {
                    ty: instrs.last().map(|e: &Expr| e.ty()).unwrap_or(Ty::Obj),
                    insts: instrs,
                }
            }

            e => {
                panic!("Unsupported expresion: {e:?}");
            }
        })
    }
}
