//! Emit C++ code

use hulkompiler_hir as hir;

/// Embed the HULK standard library
const STD: &str = include_str!("../std.cpp");

/// A basic code generator for C++
pub struct Emitter<'a> {
    unit: &'a hir::unit::Unit,
    outp: Vec<String>,
}

impl<'a> Emitter<'a> {
    /// Convert a type to a string representation
    fn ty_to_str(&self, ty: &hir::ty::Ty) -> &str {
        match ty {
            hir::ty::Ty::Obj => "HkObject",
            hir::ty::Ty::Str => "HkString",
            hir::ty::Ty::Num => "HkNumber",
            hir::ty::Ty::Bool => "HkBoolean",
        }
    }

    /// Transform the given unit
    pub fn emit(unit: &'a hir::unit::Unit) -> String {
        let mut inst = Self {
            unit,
            outp: vec![STD.into()],
        };

        for fun in &unit.funpool {
            match fun.body {
                hir::expr::FunBody::Expr(_) => inst.emit_fun(&fun),
                _ => continue,
            }
        }

        inst.outp.join("\n")
    }

    /// Emit the given function
    fn emit_fun(&mut self, fun: &hir::expr::Fun) {
        // Write the definition
        let ret = self.ty_to_str(&fun.ty);
        let name = &fun.name;
        let args = fun
            .args
            .iter()
            .map(|arg| format!("{} {}", self.ty_to_str(&arg.ty), arg.name))
            .collect::<Vec<_>>()
            .join(",");

        self.outp.push(format!("{ret} hk_{name}({args}) {{}}"));
    }
}
