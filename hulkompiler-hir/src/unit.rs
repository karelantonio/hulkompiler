//! A code unit, contains all the definition and code
//! See [`Unit`]

use crate::{
    scope::Scope,
    expr::{Expr, Const, ConstId, Fun, FunId, Var, VarId},
};

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
