//! Transpile HULK to python (3)

use crate::hir;

pub struct Emitter<'a> {
    unit: &'a hir::Unit,
}

impl<'a> Emitter<'a> {
    pub fn emit(unit: &'a hir::Unit) -> String {
        Self { unit }.generate()
    }

    fn generate(&mut self) -> String {
        // The expressions
        self.expr_to_py(&self.unit.expr)
    }

    fn value_to_str(&self, ct: &hir::Const) -> String {
        match &ct.value {
            hir::ConstValue::Num(v) => format!("{v}"),
            hir::ConstValue::Str(v) => format!("\"{v}\""),
            hir::ConstValue::Bool(v) => if *v { "True" } else { "False" }.into(),
        }
    }

    /// Convert an expresion to python, without indentation
    fn expr_to_py(&mut self, expr: &hir::Expr) -> String {
        match expr {
            hir::Expr::Const { cons, ty } => {
                // Just get the value, if it is a string
                let val = self
                    .unit
                    .lookup_const(cons)
                    .expect("Const expected to be inside Unit");
                self.value_to_str(&val)
            }
            hir::Expr::BinOp {
                op,
                ty,
                left,
                right,
            } => {
                let opchar = match op {
                    hir::Op::Add => "+",
                    hir::Op::Sub => "-",
                    hir::Op::Div => "/",
                    hir::Op::Mul => "*",
                    hir::Op::Pow => "**",
                    _ => panic!("Unsupported operator: {op:?}"),
                };

                // Make it work, then make it pretty
                format!(
                    "({}){opchar}({})",
                    self.expr_to_py(&left),
                    self.expr_to_py(&right)
                )
            }
            _ => panic!("Unsupported expr type: {:?}, dont know how to proceed", expr)
        }
    }
}
