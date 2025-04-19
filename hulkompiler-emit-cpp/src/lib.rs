//! Emit C++ code

use hulkompiler_hir as hir;

/// Embed the HULK standard library
const STD: &str = include_str!("../std.cpp");

/// To construct scopes
pub struct ScopeBuilder {
    id: usize,
    outp: Vec<Instruction>,
    vars: Vec<usize>,
}

impl ScopeBuilder {
    fn dump_to(mut self, to: &mut Vec<String>) {
        for inst in self.outp {
            match inst {
                Instruction::Line(val) => to.push(val),
                Instruction::Scope(sco) => sco.dump_to(to),
            }
        }

        // Free the variables
        /*self.vars.reverse();
        for var in self.vars {
            to.push(format!("  delete v_{var};"));
        }*/
    }
}

pub enum Instruction {
    Line(String),
    Scope(ScopeBuilder),
}

/// A way to access the result of [`Emitter::emit_expr`]
pub struct ResRef {
    name: usize,
    free: bool,
}

impl ResRef {
    fn add_if_free(&self, scope: &mut ScopeBuilder) {
        if self.free {
            scope.vars.push(self.name);
        }
    }
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
            vars: Vec::new(),
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
        let e = inst.emit_expr(&mut sco, &unit.expr);
        e.add_if_free(&mut sco);

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
            .map(|arg| format!("shp<{}> hkp_{}", self.ty_to_str(&arg.ty), arg.name))
            .collect::<Vec<_>>()
            .join(",");

        self.outp.push(format!("shp<{ret}> hk_{name}({args}) {{"));

        match &fun.body {
            hir::expr::FunBody::Expr(expr) => {
                let mut scope = self.alloc_scope();
                let last = self.emit_expr(&mut scope, expr);
                scope.dump_to(&mut self.outp);
                self.outp.push(format!("  return v_{};", last.name));
            }
            _ => (),
        }

        self.outp.push("}".into());
    }

    /// Emit an expression and return in which variable the result is (r_<RETURN>)
    fn emit_expr(&mut self, scope: &mut ScopeBuilder, expr: &hir::expr::Expr) -> ResRef {
        let mut free = true;
        let ex = match &expr {
            // A constant
            hir::expr::Expr::Const { cons, ty: _ } => {
                let cons = self
                    .unit
                    .lookup_const(cons)
                    .expect("Program is in a bad state");
                match &cons.value {
                    hir::expr::ConstValue::Num(num) => format!("std::make_shared<HkNumber>(HkNumber({num}))"),
                    hir::expr::ConstValue::Bool(b) => {
                        format!("std::make_shared<HkBoolean>(HkBoolean({}))", if *b { "true" } else { "false" })
                    }
                    hir::expr::ConstValue::Str(s) => format!("std::make_shared<HkString>(HkString(\"{s}\"))"),
                }
            }

            // An implicit cast
            hir::expr::Expr::ImplicitCast { ty, expr } => {
                // Repr if needed
                if *ty == hir::ty::Ty::Str && expr.ty() != hir::ty::Ty::Str {
                    let res = self.emit_expr(scope, expr);
                    res.add_if_free(scope);
                    format!("new HkString(v_{})", res.name)
                } else {
                    return self.emit_expr(scope, expr);
                }
            }

            // A variable read
            hir::expr::Expr::VarRead { ty: _, var } => {
                let var = self
                    .unit
                    .lookup_var(var)
                    .expect("Variable not found, program in a bad state");
                free = false;
                match var.kind {
                    hir::expr::VarKind::Local => format!("l_{}", var.id.id()),
                    hir::expr::VarKind::Param => format!("hkp_{}", var.name),
                    hir::expr::VarKind::Global => format!("hkv_{}", var.name),
                }
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
                    let res = self.emit_expr(scope, arg);
                    res.add_if_free(scope);
                    fargs.push(format!("v_{}", res.name));
                }

                format!("hk_{name}({})", fargs.join(","))
            }

            // An unary operator
            hir::expr::Expr::UnaryOp { ty: _, op, expr } => {
                let op = match op {
                    hir::ops::UOp::Neg => "-",
                    hir::ops::UOp::Not => "!",
                };

                let vr = self.emit_expr(scope, expr);
                vr.add_if_free(scope);
                format!("{op}v_{}", vr.name)
            }

            // Binary operator (concat)
            hir::expr::Expr::BinOp {
                op: hir::ops::Op::Cat,
                ty: _,
                left,
                right,
            } => {
                let left = self.emit_expr(scope, left);
                left.add_if_free(scope);
                let right = self.emit_expr(scope, right);
                right.add_if_free(scope);
                format!("v_{}->cat(v_{})", left.name, right.name)
            }

            // Binary operator (Pow)
            hir::expr::Expr::BinOp {
                op: hir::ops::Op::Pow,
                ty: _,
                left,
                right,
            } => {
                let left = self.emit_expr(scope, left);
                left.add_if_free(scope);
                let right = self.emit_expr(scope, right);
                right.add_if_free(scope);
                format!("v_{}->pow({})", left.name, right.name)
            }

            // Binary operator
            hir::expr::Expr::BinOp {
                op,
                ty: _,
                left,
                right,
            } => {
                let left = self.emit_expr(scope, left);
                left.add_if_free(scope);
                let right = self.emit_expr(scope, right);
                right.add_if_free(scope);
                let opch = match op {
                    hir::ops::Op::Add => "opadd",
                    hir::ops::Op::Sub => "opsub",
                    hir::ops::Op::Mul => "opmult",
                    hir::ops::Op::Div => "opdiv",
                    hir::ops::Op::And => "opand",
                    hir::ops::Op::Or => "opor",
                    hir::ops::Op::Eq => "opeq",
                    hir::ops::Op::Neq => "opneq",
                    hir::ops::Op::Ge => "opgte",
                    hir::ops::Op::Gt => "opgt",
                    hir::ops::Op::Le => "oplte",
                    hir::ops::Op::Lt => "oplt",
                    hir::ops::Op::Pow | hir::ops::Op::Cat => unreachable!(),
                };

                format!("{opch}(v_{},v_{})", left.name, right.name)
            }

            // Other binary operator
            _ => panic!("Dont know how to process {expr:?}"),
        };
        let res = self.alloc_var();
        let ty = self.ty_to_str(&expr.ty());
        scope
            .outp
            .push(Instruction::Line(format!("  shp<{ty}> v_{res} = {ex};")));

        ResRef { name: res, free }
    }
}
