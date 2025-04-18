//! The scope. See [`Scope`]
//! This module contains an utility to check which variables are into scope and add/remove easily

use std::collections::BTreeMap;
use crate::{
    expr::{VarId, Var, VarKind},
    ty::Ty,
};


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

    /// Push into the scope a variable (which already existed, so we dont need to create a new one)
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

    /// Create a new variable and push into the scope, if you dont want to create a new one use [`Self::push_oldvar`]
    /// Returns the variable ID of the new variable
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

    /// Pop the last added variable in the scope, returns the popped Variable ID, or nothing if
    /// empty
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
