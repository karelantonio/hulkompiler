//! Emit C++ code

use hulkompiler_hir as hir;

/// Embed the HULK standard library
const STD: &str = include_str!("../std.cpp");

/// To construct scopes
pub struct ScopeBuilder {
    id: usize,
    outp: Vec<Instruction>,
}

impl ScopeBuilder {
    fn dump_to(self, to: &mut Vec<String>) {
        for inst in self.outp {
            match inst {
                Instruction::Line(val) => to.push(val),
                Instruction::Scope(sco) => sco.dump_to(to),
            }
        }
    }
}

pub enum Instruction {
    Line(String),
    Scope(ScopeBuilder),
}

/// A basic code generator for C++
pub struct Emitter<'a> {
    unit: &'a hir::unit::Unit,
    outp: Vec<String>,
    scopecnt: usize,
    varcnt: usize,
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

    /// Allocate a new var (variable)
    fn alloc_var(&mut self) -> usize {
        let id = self.varcnt;
        self.varcnt += 1;
        id
    }

    /// Allocate new scope
    fn alloc_scope(&mut self) -> ScopeBuilder {
        let id = self.scopecnt;
        self.scopecnt += 1;
        ScopeBuilder {
            id,
            outp: Vec::new(),
        }
    }

    /// Transform the given unit
    pub fn emit(unit: &'a hir::unit::Unit) -> String {
        let mut inst = Self {
            unit,
            outp: vec![STD.into()],
            scopecnt: 0,
            varcnt: 0,
        };

        for fun in &unit.funpool {
            match fun.body {
                hir::expr::FunBody::Expr(_) => inst.emit_fun(&fun),
                _ => continue,
            }

            inst.scopecnt = 0;
            inst.varcnt = 0;
        }

        // Now the entry point
        let mut sco = inst.alloc_scope();
        let _ = inst.emit_expr(&mut sco, &unit.expr);

        inst.outp.push("int main() {".into());
        sco.dump_to(&mut inst.outp);
        inst.outp.push("  return EXIT_SUCCESS;".into());
        inst.outp.push("}".into());

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

        self.outp.push(format!("{ret} hk_{name}({args}) {{"));

        match &fun.body {
            hir::expr::FunBody::Expr(expr) => {
                let mut scope = self.alloc_scope();
                self.emit_expr(&mut scope, expr);
                scope.dump_to(&mut self.outp);
            }
            _ => (),
        }

        self.outp.push("}".into());
    }

    /// Emit an expression and return in which variable the result is (r_<RETURN>)
    fn emit_expr(&mut self, scope: &mut ScopeBuilder, expr: &hir::expr::Expr) -> usize {
        let res = self.alloc_var();
        let ex = match &expr {
            // A constant
            hir::expr::Expr::Const { cons, ty: _ } => {
                let cons = self
                    .unit
                    .lookup_const(cons)
                    .expect("Program is in a bad state");
                match &cons.value {
                    hir::expr::ConstValue::Num(num) => format!("HkNumber({num})"),
                    hir::expr::ConstValue::Bool(b) => {
                        format!("HkBoolean({})", if *b { "true" } else { "false" })
                    }
                    hir::expr::ConstValue::Str(s) => format!("HkString(\"{s}\")"),
                }
            }

            // An implicit cast
            hir::expr::Expr::ImplicitCast { ty: _, expr } => {
                format!("v_{}", self.emit_expr(scope, expr))
            }

            // A variable read
            hir::expr::Expr::VarRead { ty: _, var } => {
                let var = self
                    .unit
                    .lookup_var(var)
                    .expect("Variable not found, program in a bad state");
                format!("hkv_{}", var.name)
            }

            // A function call
            hir::expr::Expr::Call { fun, ty: _, args } => {
                let name = &self
                    .unit
                    .lookup_fun(fun)
                    .expect("Function not found, program in a bad state")
                    .name;

                let mut fargs = Vec::new();
                for arg in args {
                    fargs.push(format!("v_{}", self.emit_expr(scope, arg)));
                }

                format!("hk_{name}({})", fargs.join(","))
            }

            _ => panic!("Dont know how to process {expr:?}"),
        };
        let ty = self.ty_to_str(&expr.ty());
        scope
            .outp
            .push(Instruction::Line(format!("  {ty} v_{res} = {ex};")));

        res
    }
}
