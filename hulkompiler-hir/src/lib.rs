//! The high-level intermediate representation of the language
//! Makes easy the type-checking phase :)

use hulkompiler_ast as ast;
use hulkompiler_ast::Loc;
use hulkompiler_sourcehint::{make, LocError};
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
    Le,
    Ge,
    Lt,
    Gt,
    Eq,
    Neq,
    And,
    Or,
}

impl Op {
    fn is_arithmetic(&self) -> bool {
        match self {
            Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Pow
            | Self::Le
            | Self::Ge
            | Self::Lt
            | Self::Gt => true,
            _ => false,
        }
    }

    fn is_comparison(&self) -> bool {
        match self {
            Self::Le | Self::Lt | Self::Ge | Self::Gt | Self::Eq | Self::Neq => true,
            _ => false,
        }
    }

    fn is_logic(&self) -> bool {
        match self {
            Self::And | Self::Or => true,
            _ => false,
        }
    }
}

/// Unary operator
#[derive(Debug)]
pub enum UOp {
    Neg, // additive inverse
    Not, // logical negation
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

/// A function ID (for references)
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub struct FunId(usize);

/// A variable ID
#[derive(Debug, Clone, Copy)]
pub struct VarId(usize);

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

    pub fn push_oldvar(&mut self, name: &str, id: &VarId) {
        if let Some(oldid) = self.reverse_vars.insert(name.into(), *id) {
            self.deltas.push(ScopeDelta::Shadowed {
                old: oldid,
                new: *id,
            });
        } else {
            self.deltas.push(ScopeDelta::New { id: *id })
        }
    }

    pub fn push_var(&mut self, name: &str, ty: Ty, kind: VarKind) -> VarId {
        let id = self.vars.len();

        self.vars.push(Var {
            id: VarId(id),
            kind,
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

    #[error("Logic operators can only be applied to boolean expressions")]
    LogicOperOnNotBoolean {
        #[source]
        loc: LocError,
    },

    #[error("The condition expression in a branching instruction (if-elif-else) must be of type boolean")]
    CondNotBoolean {
        #[source]
        loc: LocError,
    },

    #[error("The condition of a loop must be of type boolean")]
    LoopCondNotBoolean {
        #[source]
        loc: LocError,
    },

    #[error("The variable {name} referenced in this reassignment does not exist")]
    ReassignVarDoesNotExist {
        name: String,
        #[source]
        loc: LocError,
    },

    #[error("Reassigned value does not conform the variable's original type ({expr:?} cannot be interpreted as {vart:?})")]
    ReassignDoesNotConform { expr: Ty, vart: Ty, loc: LocError },
    
    #[error("Cannot re-assign the value of a global variable, these are like constants. Try shadowing it with a variable with the same name")]
    ReassignGlobal {
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

    /// The only two possible boolean constants
    fn push_boolean_constants(&mut self) {
        // This should be the first constants to be added, in order to access them easy
        let id = self.constpool.len();
        if id != 0 {
            panic!("You should add the boolean constants first!");
        }
        self.constpool.push(Const {
            id: ConstId(0),
            value: ConstValue::Bool(false),
        });
        self.constpool.push(Const {
            id: ConstId(1),
            value: ConstValue::Bool(true),
        });
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

    fn get_boolean_const(&mut self, val: bool) -> ConstId {
        ConstId(if val { 1 } else { 0 })
    }

    fn make_loc_err(&self, loc: &Loc) -> LocError {
        make(&loc.content, loc.start.line, loc.start.col, loc.end.line, loc.end.col)
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

            UOp::Not => {
                if expr.ty() != Ty::Bool {
                    return Err(TypeError::LogicOperOnNotBoolean {
                        loc: self.make_loc_err(loc),
                    });
                }
            }
        }

        Ok(())
    }

    /// Find the least common ancestor of both types
    fn find_lca(&self, a: &Ty, b: &Ty) -> Ty {
        if a == b {
            *a
        } else {
            Ty::Obj
        }
    }

    /// Reserve a function param
    fn reserve_fn_param(&mut self, name: &str, ty: &Ty) -> FunArg {
        let arg = FunArg {
            name: name.into(),
            ty: *ty,
            id: self.scope.push_var(name, *ty, VarKind::Param),
        };

        self.scope.undo();

        arg
    }

    /// Transform the given expresion to the given unit
    pub fn transform(ast: &[ast::RootElem]) -> TResult<Unit> {
        // Transform this expr to our expr
        let mut tr = Self::default();
        tr.push_boolean_constants();

        // Add the Standard library functions
        let params = [tr.reserve_fn_param("val", &Ty::Obj)];
        tr.add_stub_fun("print", Ty::Obj, &params, true);
        // -- math functions --
        let params = [tr.reserve_fn_param("x", &Ty::Num)];
        tr.add_stub_fun("sqrt", Ty::Num, &params, true);
        tr.add_stub_fun("sin", Ty::Num, &params, true);
        tr.add_stub_fun("cos", Ty::Num, &params, true);
        tr.add_stub_fun("exp", Ty::Num, &params, true);
        tr.add_stub_fun("floor", Ty::Num, &params, true);
        let params = [
            tr.reserve_fn_param("base", &Ty::Num),
            tr.reserve_fn_param("arg", &Ty::Num),
        ];
        tr.add_stub_fun("log", Ty::Num, &params, true);
        tr.add_stub_fun("rand", Ty::Num, &[], true);
        // Standard library variables
        tr.scope.push_var("PI", Ty::Num, VarKind::Global);

        // First, add the stub functions
        tr.create_fun_stubs(ast)?;

        // Now create the functions
        for elem in ast {
            let ast::RootElem::FunDecl(fun) = elem else {
                continue;
            };

            tr.process_fn(&fun)?;
        }

        // Now the actual global expression
        let mut glob = None;

        for elem in ast {
            let ast::RootElem::Statement(expr) = elem else {
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

    fn create_fun_stubs(&mut self, ast: &[ast::RootElem]) -> TResult<()> {
        for elem in ast {
            let ast::RootElem::FunDecl(fun) = elem else {
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
                    let ty = self.str_to_ty(&argty).ok_or(TypeError::UnknownTy {
                        loc: self.make_loc_err(&fun.loc),
                        name: aname.clone(),
                    })?;
                    Ok(self.reserve_fn_param(&aname, &ty))
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

    fn to_op(&self, op: &ast::BinOp) -> Op {
        match op {
            ast::BinOp::Add => Op::Add,
            ast::BinOp::Sub => Op::Sub,
            ast::BinOp::Mult => Op::Mul,
            ast::BinOp::Div => Op::Div,
            ast::BinOp::Pwr => Op::Pow,
            ast::BinOp::Cat => Op::Cat,
            ast::BinOp::Le => Op::Le,
            ast::BinOp::Ge => Op::Ge,
            ast::BinOp::Lt => Op::Lt,
            ast::BinOp::Gt => Op::Gt,
            ast::BinOp::Eq => Op::Eq,
            ast::BinOp::Neq => Op::Neq,
            ast::BinOp::And => Op::And,
            ast::BinOp::Or => Op::Or,
        }
    }

    fn to_unary_op(&self, op: &ast::UnaryOp) -> UOp {
        match op {
            ast::UnaryOp::Neg => UOp::Neg,
            ast::UnaryOp::Not => UOp::Not,
        }
    }

    fn process_fn(&mut self, fun: &ast::FunDecl) -> TResult<()> {
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
            self.scope.push_oldvar(&arg.name, &arg.id);
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

    fn to_expr(&mut self, expr: &ast::Expr) -> TResult<Expr> {
        Ok(match expr {
            ast::Expr::Num(_, v) => Expr::Const {
                ty: Ty::Num,
                cons: self.push_const_num(*v),
            },

            ast::Expr::Str(_, v) => Expr::Const {
                ty: Ty::Str,
                cons: self.push_const_str(&v),
            },

            ast::Expr::Boolean(_, v) => Expr::Const {
                ty: Ty::Bool,
                cons: self.get_boolean_const(*v),
            },

            ast::Expr::BinOpExpr(binop) => {
                let op = self.to_op(&binop.op);
                let left = self.to_expr(&binop.left)?;
                let right = self.to_expr(&binop.right)?;

                self.can_apply(&op, &left, &right, &binop.loc)?;

                let ty = if op.is_comparison() || op.is_logic() {
                    Ty::Bool
                } else if op == Op::Cat {
                    Ty::Str
                } else if op.is_arithmetic() {
                    Ty::Num
                } else {
                    panic!("Unknown operator: {op:?}")
                };

                Expr::BinOp {
                    ty,
                    op,
                    left: left.into(),
                    right: right.into(),
                }
            }

            ast::Expr::UnaryOpExpr(crate::ast::UnaryOpExpr { loc, op, expr }) => {
                let op = self.to_unary_op(&op);
                let expr = self.to_expr(expr)?;

                self.can_apply_unary(&op, &expr, &loc)?;

                Expr::UnaryOp {
                    ty: Ty::Num,
                    op,
                    expr: Box::new(expr),
                }
            }

            ast::Expr::FunCallExpr(fun) => {
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

            ast::Expr::BlockExpr(crate::ast::BlockExpr { exprs, .. }) => {
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

            ast::Expr::Id(loc, name) => {
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

            ast::Expr::VarDeclExpr(crate::ast::VarDeclExpr {
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
                let var = self.scope.push_var(&name, ty, VarKind::Local);

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

                // Pop the var
                self.scope.undo();

                Expr::VarDecl {
                    ty,
                    var,
                    expr: Box::new(expr),
                    scope: Box::new(scope),
                }
            }

            ast::Expr::IfExpr(crate::ast::IfExpr {
                loc,
                ifarm,
                elsearm,
                cond,
            }) => {
                let cond = self.to_expr(cond)?;

                if cond.ty() != Ty::Bool {
                    return Err(TypeError::CondNotBoolean {
                        loc: self.make_loc_err(loc),
                    });
                }

                let mut left = self.to_expr(ifarm)?;
                let mut right = self.to_expr(elsearm)?;

                // Find the LCA of both return types
                let ty = self.find_lca(&left.ty(), &right.ty());

                // Maybe insert some implicit casts
                if left.ty() != ty {
                    left = Expr::ImplicitCast {
                        ty: ty.clone(),
                        expr: Box::new(left),
                    };
                }

                if right.ty() != ty {
                    right = Expr::ImplicitCast {
                        ty: ty.clone(),
                        expr: Box::new(right),
                    }
                }

                Expr::Branch {
                    ty,
                    cond: Box::new(cond),
                    ontrue: Box::new(left),
                    onfalse: Box::new(right),
                }
            }

            ast::Expr::WhileExpr(crate::ast::WhileExpr { body, cond, loc }) => {
                let cond = self.to_expr(cond)?;

                if cond.ty() != Ty::Bool {
                    return Err(TypeError::LoopCondNotBoolean {
                        loc: self.make_loc_err(loc),
                    });
                }

                let body = self.to_expr(body)?;

                Expr::Loop {
                    ty: body.ty(),
                    cond: Box::new(cond),
                    body: Box::new(body),
                }
            }

            ast::Expr::Reassign(crate::ast::Reassign { expr, name, loc }) => {
                let var = *self.scope.reverse_vars.get(name).ok_or(
                    TypeError::ReassignVarDoesNotExist {
                        name: name.into(),
                        loc: self.make_loc_err(loc),
                    },
                )?;

                let vk = self.scope.vars[var.0].kind.clone();

                if matches!(vk, VarKind::Global) {
                    return Err(TypeError::ReassignGlobal {
                        loc: self.make_loc_err(loc),
                    });
                }

                // Lookup the variable type
                let vt = self.scope.vars[var.0].ty;

                // The body
                let mut expr = self.to_expr(expr)?;
                let exprt = expr.ty();

                // Check if type conforms
                if !self.ty_conforms(&exprt, &vt) {
                    return Err(TypeError::ReassignDoesNotConform {
                        expr: exprt,
                        vart: vt,
                        loc: self.make_loc_err(loc),
                    });
                }

                // Add an implicit cast?
                if vt != exprt {
                    expr = Expr::ImplicitCast {
                        ty: vt.clone(),
                        expr: Box::new(expr),
                    };
                }

                // Now reassign
                Expr::Reassign {
                    ty: vt,
                    var,
                    expr: Box::new(expr),
                }
            }
        })
    }
}
