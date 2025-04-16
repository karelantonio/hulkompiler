//! The high-level intermediate representation of the language
//! Makes easy the type-checking phase :)

use crate::{
    ast::Loc,
    sourcehint::{make, LocError},
};
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
#[derive(Debug, Eq, PartialEq)]
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

/// Unary operator
#[derive(Debug)]
pub enum UOp {
    Neg,
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
        }
    }

    pub fn is_const(&self) -> bool {
        matches!(self, Self::Const { .. })
    }
}

/// A function ID (for references)
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct FunId(usize);

/// A variable ID
#[derive(Debug, Clone, Copy)]
pub struct VarId(usize);

/// A variable
#[derive(Debug, Clone)]
pub struct Var {
    pub id: VarId,
    pub name: String,
    pub ty: Ty,
}

/// A scope delta, like add a variable
#[derive(Debug, Clone)]
pub enum ScopeDelta {
    New { id: VarId },
    Shadowed { old: VarId, new: VarId },
}

/// A scope which stores variables and can mutate
#[derive(Debug, Default, Clone)]
pub struct Scope {
    pub varcnt: usize,
    pub vars: Vec<Var>,
    pub reverse_vars: BTreeMap<String, VarId>,
    pub deltas: Vec<ScopeDelta>,
}

impl Scope {
    pub fn push_var(&mut self, name: &str, ty: Ty) -> VarId {
        let id = self.vars.len();

        self.vars.push(Var {
            id: VarId(id),
            name: name.into(),
            ty,
        });

        if let Some(oldid) = self.reverse_vars.insert(name.into(), VarId(id)) {
            self.deltas.push(ScopeDelta::Shadowed {
                old: oldid,
                new: VarId(id),
            });
        } else {
            self.deltas.push(ScopeDelta::New { id: VarId(id) })
        }

        VarId(id)
    }

    pub fn undo(&mut self) -> Option<VarId> {
        let Some(delta) = self.deltas.pop() else {
            return None;
        };

        Some(match delta {
            ScopeDelta::New { id: VarId(id) } => {
                let name = &self.vars[id].name;

                let _ = self.reverse_vars.remove(name);
                VarId(id)
            }
            ScopeDelta::Shadowed {
                old,
                new: VarId(new),
            } => {
                let name = &self.vars[new].name;

                let _ = self.reverse_vars.insert(name.into(), old);
                VarId(new)
            }
        })
    }
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
    pub scope: Scope,
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

    pub fn lookup_var(&self, VarId(vid): &VarId) -> Option<&Var> {
        self.scope.vars.get(*vid)
    }
}

/// Errors that may happend during the type checking phase
#[derive(Debug, Error)]
pub enum TypeError {
    #[error("Unknown reference to function: {name}")]
    UnknownFun {
        name: String,
        #[source]
        loc: LocError,
    },

    #[error("Unknown reference to type: {name}")]
    UnknownTy {
        name: String,
        #[source]
        loc: LocError,
    },

    #[error("Unknown reference to variable: {name}")]
    UnknownVar {
        name: String,
        #[source]
        loc: LocError,
    },

    #[error("Both expressions must be numbers in order to apply an arithmetic operator. Did you mean concatenate '@' ?")]
    ExprMustBeNumbers {
        #[source]
        loc: LocError,
    },

    #[error("Expression must result in a number in order to apply that unary operator")]
    UnaryExprMustBeNumber {
        #[source]
        loc: LocError,
    },

    #[error("Required {req} args for function {fname} but {found} were provided")]
    InvalidNumberOfArgs {
        #[source]
        loc: LocError,
        req: usize,
        fname: String,
        found: usize,
    },

    #[error("{idx}th argument ({name}) type mismatch, the given type ({found:?}) can not be interpreted as {exp:?} (does not conform {exp:?})")]
    FnArgMismatch {
        #[source]
        loc: LocError,
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
    MultipleGlobExpr {
        #[source]
        loc: LocError,
    },

    #[error("A function with the same name ({name}) is already defined, try another name or use a prefix")]
    DupdFun {
        name: String,
        #[source]
        loc: LocError,
    },

    #[error("Function arguments with no type are not supported yet, try specifying the type: `..{name}: Object`")]
    FnArgNoType {
        name: String,
        #[source]
        loc: LocError,
    },

    #[error("Function without return type are not supported yet: {name}, try specifying a type: `): Object`")]
    FnNoRetType {
        name: String,
        #[source]
        loc: LocError,
    },

    #[error("Function return mismatch, body type ({bod}) can not be interpreted as ({ret}) (does not conform {ret})")]
    FnRetMismatch {
        ret: String,
        bod: String,
        #[source]
        loc: LocError,
    },

    #[error("The result of the variable {name}'s expression does not result in a type conforming the declared type ({exprty:?} cannot be interpreted as {declty:?})")]
    VarExprNotConformsTy {
        name: String,
        declty: Ty,
        exprty: Ty,
        #[source]
        loc: LocError,
    },
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
    scope: Scope,
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

    fn make_loc_err(&self, loc: &Loc) -> LocError {
        make(&loc.content, &loc.start, &loc.end)
    }

    /// Check if can apply the given operator to these expressions
    fn can_apply(&self, op: &Op, left: &Expr, right: &Expr, loc: &Loc) -> TResult<()> {
        // Check if is arithmetic
        if op.is_arithmetic() && (left.ty() != Ty::Num || right.ty() != Ty::Num) {
            return Err(TypeError::ExprMustBeNumbers {
                loc: self.make_loc_err(loc),
            });
        }

        Ok(())
    }

    /// Check if the given unary operator can be applied to that expression
    fn can_apply_unary(&self, op: &UOp, expr: &Expr, loc: &Loc) -> TResult<()> {
        match op {
            UOp::Neg => {
                if expr.ty() != Ty::Num {
                    return Err(TypeError::UnaryExprMustBeNumber {
                        loc: self.make_loc_err(loc),
                    });
                }
            }
        }

        Ok(())
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
        tr.add_stub_fun(
            "floor",
            Ty::Num,
            &[FunArg {
                name: "arg0".into(),
                ty: Ty::Num,
            }],
            true,
        );
        // Standard library variables
        tr.scope.push_var("PI", Ty::Num);

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
                return Err(TypeError::MultipleGlobExpr {
                    loc: tr.make_loc_err(elem.loc()),
                });
            }

            glob = Some(tr.to_expr(expr)?);
        }

        Ok(Unit {
            constpool: tr.constpool,
            expr: glob.ok_or(TypeError::NoGlobExpr)?,
            funpool: tr.funpool,
            scope: tr.scope.clone(),
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
                        loc: self.make_loc_err(&fun.loc),
                        name: aname.clone(),
                    })?;
                    Ok(FunArg {
                        name: aname.clone(),
                        ty: self.str_to_ty(&argty).ok_or(TypeError::UnknownTy {
                            loc: self.make_loc_err(&fun.loc),
                            name: aname.into(),
                        })?,
                    })
                })
                .collect::<Result<Vec<_>, TypeError>>()?;

            let ret = fun.ret.clone().ok_or(TypeError::FnNoRetType {
                loc: self.make_loc_err(&fun.loc),
                name: fun.name.clone(),
            })?;
            let ret = self.str_to_ty(&ret).ok_or(TypeError::UnknownTy {
                loc: self.make_loc_err(&fun.loc),
                name: ret.into(),
            })?;

            self.add_stub_fun(&fun.name, ret, &args, false);
        }

        Ok(())
    }

    fn to_op(&self, op: &crate::ast::BinOp) -> Op {
        match op {
            crate::ast::BinOp::Add => Op::Add,
            crate::ast::BinOp::Sub => Op::Sub,
            crate::ast::BinOp::Mult => Op::Mul,
            crate::ast::BinOp::Div => Op::Div,
            crate::ast::BinOp::Pwr => Op::Pow,
            crate::ast::BinOp::Cat => Op::Cat,
        }
    }

    fn to_unary_op(&self, op: &crate::ast::UnaryOp) -> UOp {
        match op {
            crate::ast::UnaryOp::Neg => UOp::Neg,
        }
    }

    fn process_fn(&mut self, fun: &crate::ast::FunDecl) -> TResult<()> {
        // Check is not already defined (no stub)
        let FunId(fid) = self.reverse_fun[&fun.name]; // Its ok to panic, because we actually EXPECT
                                                      // fun.name to be a stub

        if !self.is_stub(&FunId(fid)) {
            return Err(TypeError::DupdFun {
                loc: self.make_loc_err(&fun.loc),
                name: fun.name.clone(),
            });
        }

        // Add variables to scope
        for arg in self.funpool[fid].args.iter() {
            self.scope.push_var(&arg.name, arg.ty);
        }

        // Transform the function body
        let body = self.to_expr(&fun.body)?;

        // Undo (maybe drop?)
        for _ in 0..self.funpool[fid].args.len() {
            self.scope.undo();
        }

        // Add the function to the pool
        self.update_fn_body(&FunId(fid), FunBody::Expr(body));
        self.unstub(&FunId(fid));

        Ok(())
    }

    fn to_expr(&mut self, expr: &crate::ast::Expr) -> TResult<Expr> {
        Ok(match expr {
            crate::ast::Expr::Num(_, v) => Expr::Const {
                ty: Ty::Num,
                cons: self.push_const_num(*v),
            },

            crate::ast::Expr::Str(_, v) => Expr::Const {
                ty: Ty::Str,
                cons: self.push_const_str(&v),
            },

            crate::ast::Expr::BinOpExpr(binop) => {
                let op = self.to_op(&binop.op);
                let left = self.to_expr(&binop.left)?;
                let right = self.to_expr(&binop.right)?;

                self.can_apply(&op, &left, &right, &binop.loc)?;

                let ty = if op.is_arithmetic() {
                    Ty::Num
                } else if op == Op::Cat {
                    Ty::Str
                } else {
                    panic!("Unknown type: {op:?}")
                };

                Expr::BinOp {
                    ty,
                    op,
                    left: left.into(),
                    right: right.into(),
                }
            }

            crate::ast::Expr::UnaryOpExpr(crate::ast::UnaryOpExpr { loc, op, expr }) => {
                let op = self.to_unary_op(&op);
                let expr = self.to_expr(expr)?;

                self.can_apply_unary(&op, &expr, &loc)?;

                Expr::UnaryOp {
                    ty: Ty::Num,
                    op,
                    expr: Box::new(expr),
                }
            }

            crate::ast::Expr::FunCallExpr(fun) => {
                // Check if the function exists
                let FunId(funid) = self
                    .reverse_fun
                    .get(&fun.name)
                    .ok_or(TypeError::UnknownFun {
                        loc: self.make_loc_err(&fun.loc),
                        name: fun.name.clone(),
                    })?
                    .clone();

                let ty = {
                    let funref = &self.funpool[funid];

                    if fun.args.len() != funref.args.len() {
                        return Err(TypeError::InvalidNumberOfArgs {
                            loc: self.make_loc_err(&fun.loc),
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
                            loc: self.make_loc_err(arg.loc()),
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

            crate::ast::Expr::BlockExpr(crate::ast::BlockExpr { exprs, .. }) => {
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

            crate::ast::Expr::Id(loc, name) => {
                let VarId(vid) =
                    self.scope
                        .reverse_vars
                        .get(name)
                        .ok_or(TypeError::UnknownVar {
                            name: name.into(),
                            loc: self.make_loc_err(&loc),
                        })?;
                let var = &self.scope.vars[*vid];

                Expr::VarRead {
                    ty: var.ty,
                    var: var.id,
                }
            }

            crate::ast::Expr::VarDeclExpr(crate::ast::VarDeclExpr {
                expr,
                scope,
                name,
                declty,
                loc,
            }) => {
                // First analyze the expression
                let expr = self.to_expr(expr)?;

                // Now we infer if required
                let ty = match declty {
                    Some(ty) => {
                        let declty = self.str_to_ty(&ty).ok_or(TypeError::UnknownTy {
                            name: ty.to_string(),
                            loc: self.make_loc_err(&loc),
                        })?;

                        if !self.ty_conforms(&expr.ty(), &declty) {
                            return Err(TypeError::VarExprNotConformsTy {
                                name: name.into(),
                                declty,
                                exprty: expr.ty().clone(),
                                loc: self.make_loc_err(loc),
                            });
                        }

                        declty
                    }
                    _ => expr.ty(),
                };

                // Declare the var
                let var = self.scope.push_var(&name, ty);

                let expr = if ty != expr.ty() {
                    Expr::ImplicitCast {
                        ty,
                        expr: Box::new(expr),
                    }
                } else {
                    expr
                };

                // The body
                let scope = self.to_expr(scope)?;

                Expr::VarDecl {
                    ty,
                    var,
                    expr: Box::new(expr),
                    scope: Box::new(scope),
                }
            }
        })
    }
}
