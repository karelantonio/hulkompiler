//! Transpile HULK to python (3)

use crate::hir;

const STD: &[&str] = &[
    "# Standard library of HULK",
    "from math import sin, cos, sqrt, exp, log, pi, floor",
    "from random import uniform",
    "def hk_print(arg:object)->object:",
    " print(arg)",
    " return object()",
    "def hk_sqrt(n:float)->float: return sqrt(n)",
    "def hk_sin(n:float)->float: return sin(n)",
    "def hk_cos(n:float)->float: return cos(n)",
    "def hk_exp(n:float)->float: return exp(n)",
    "def hk_log(b:float,a:float)->float: return log(a, base=b)",
    "def hk_floor(x:float)->float: return floor(x)",
    "def hk_rand()->float: return uniform(0, 1)",
    "hkv_PI = pi",
    "# End of HULK standard library",
    "# Begin of prelude",
    "def whileinstr(cond:callable,body:callable):",
    " while cond(): body()",
    "# End of prelude",
];

/// Convert a type to string
fn ty_to_str(ty: &hir::Ty) -> &str {
    match ty {
        hir::Ty::Obj => "object",
        hir::Ty::Str => "str",
        hir::Ty::Num => "float",
        hir::Ty::Bool => "bool",
    }
}

struct PyFun {
    name: String,
    ret: String,
    params: Vec<(String, String)>,
    vars: Vec<usize>,
    code: Vec<String>,
}

struct PyFunBuilder<'a> {
    pf: PyFun,
    unit: &'a hir::Unit,
    file: &'a mut PyFileBuilder,
}

impl<'a> PyFunBuilder<'a> {
    fn alloc_var(&mut self, vid: &hir::VarId) -> String {
        self.pf.vars.push(vid.id());
        format!("hks_{}", vid.id())
    }

    fn lookup_var(&mut self, vid: &hir::VarId) -> String {
        format!("hks_{}", vid.id())
    }

    /// The program entry point
    fn build_entry(unit: &'a hir::Unit, file: &'a mut PyFileBuilder, expr: &hir::Expr) {
        let pf = PyFun {
            name: "main".into(),
            ret: "None".into(),
            params: vec![],
            vars: vec![],
            code: vec![],
        };
        let mut slf = Self { pf, unit, file };
        slf.write_expr(expr);
        slf.file.funcs.push(slf.pf);
    }

    /// Build a function
    fn build(unit: &'a hir::Unit, file: &'a mut PyFileBuilder, fun: &hir::Fun) {
        let pf = PyFun {
            name: format!("hk_{}", fun.name),
            ret: ty_to_str(&fun.ty).into(),
            params: fun
                .args
                .iter()
                .map(|arg| (format!("hkp_{}", arg.name), ty_to_str(&arg.ty).into()))
                .collect(),
            vars: vec![],
            code: vec![],
        };

        Self::build_pyfun(unit, file, pf, &fun.body)
    }

    /// Build a pyfunction
    fn build_pyfun(
        unit: &'a hir::Unit,
        file: &'a mut PyFileBuilder,
        pf: PyFun,
        body: &hir::FunBody,
    ) {
        let mut slf = Self { pf, unit, file };

        match body {
            hir::FunBody::Expr(e) => slf.write_expr(&e),
            hir::FunBody::Std => slf.pf.code.push("pass #Defined in STD".into()),
            hir::FunBody::Native => slf.pf.code.push("pass #Native".into()),
        }

        slf.file.funcs.push(slf.pf);
    }

    fn expr_to_str(&mut self, expr: &hir::Expr) -> String {
        match expr {
            // Ignore, python makes it easy
            hir::Expr::ImplicitCast { ty: _, expr } => self.expr_to_str(expr),

            // A constant
            hir::Expr::Const { cons, ty: _ } => {
                // lookup the const in the unit:
                let cons = self
                    .unit
                    .lookup_const(&cons)
                    .expect("Constant did not exist, program is in a bad state.");
                match &cons.value {
                    hir::ConstValue::Num(nu) => format!("{nu}"),
                    hir::ConstValue::Bool(bo) => if *bo { "True" } else { "False" }.into(),
                    hir::ConstValue::Str(s) => format!("\"{s}\""),
                }
            }

            // Access a variable
            hir::Expr::VarRead { ty: _, var } => {
                let var = self
                    .unit
                    .lookup_var(&var)
                    .expect("Variable did not exist, program is in a bad state.");
                match &var.kind {
                    hir::VarKind::Local => self.lookup_var(&var.id),
                    hir::VarKind::Global => format!("hkv_{}", var.name),
                    hir::VarKind::Param => format!("hkp_{}", var.name),
                }
            }

            // A function call
            hir::Expr::Call { fun, ty: _, args } => {
                // First lookup
                let fun = self
                    .unit
                    .lookup_fun(&fun)
                    .expect("Function did not exist, program is in a bad state");
                let args: Vec<_> = args.iter().map(|a| self.expr_to_str(a)).collect();

                format!("hk_{}({})", fun.name, args.join(","))
            }

            // An unary expression
            hir::Expr::UnaryOp { ty: _, op, expr } => {
                let opch = match op {
                    hir::UOp::Neg => "-",
                    hir::UOp::Not => "not",
                };
                let expr = self.expr_to_str(expr);

                format!("{opch}({expr})")
            }

            // A binary expression (Concat)
            hir::Expr::BinOp {
                op: hir::Op::Cat,
                ty: _,
                left,
                right,
            } => {
                let lrepr = self.expr_to_str(left);
                let rrepr = self.expr_to_str(right);
                let left = if left.ty() != hir::Ty::Str {
                    format!("str({lrepr})")
                } else {
                    format!("({lrepr})")
                };
                let right = if right.ty() != hir::Ty::Str {
                    format!("str({rrepr})")
                } else {
                    format!("({rrepr})")
                };

                format!("{left}+{right}")
            }

            // A binary expression
            hir::Expr::BinOp {
                op,
                ty: _,
                left,
                right,
            } => {
                let lrepr = self.expr_to_str(left);
                let rrepr = self.expr_to_str(right);
                let opch = match op {
                    hir::Op::Add => "+",
                    hir::Op::Sub => "-",
                    hir::Op::Mul => "*",
                    hir::Op::Div => "/",
                    hir::Op::Pow => "**",
                    hir::Op::Le => "<=",
                    hir::Op::Lt => "<",
                    hir::Op::Ge => ">=",
                    hir::Op::Gt => ">",
                    hir::Op::Eq => "==",
                    hir::Op::Neq => "!=",
                    hir::Op::And => "and",
                    hir::Op::Or => "or",
                    hir::Op::Cat => unreachable!(),
                };

                format!("({lrepr}){opch}({rrepr})")
            }

            // A group of expressions
            hir::Expr::Block { ty: _, insts } => {
                let res = insts
                    .iter()
                    .map(|e| self.expr_to_str(e))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("[{res}][-1]")
            }

            // A variable declaration
            hir::Expr::VarDecl {
                ty: _,
                var,
                expr,
                scope,
            } => {
                let assi = self.expr_to_str(expr);
                let sco = self.expr_to_str(scope);

                let name = self.lookup_var(var);

                format!("[{name}:={assi},{sco}][1]")
            }

            // A conditional expression
            hir::Expr::Branch { ty: _, cond, ontrue, onfalse } => {
                let cond = self.expr_to_str(cond);
                let ontrue = self.expr_to_str(ontrue);
                let onfalse = self.expr_to_str(onfalse);

                format!("({ontrue})if({cond})else({onfalse})")
            }

            // A loop structure
            hir::Expr::Loop { ty: _, cond, body } => {
                let cond = self.expr_to_str(cond);
                let body = self.expr_to_str(body);
                format!("whileinstr(lambda:{cond},lambda:{body})")
            }
        }
    }

    fn write_expr(&mut self, expr: &hir::Expr) {
        // The root element of the function
        match expr {
            hir::Expr::ImplicitCast { ty: _, expr } => self.write_expr(&expr),
            hir::Expr::Block { ty: _, insts } => {
                for inst in insts {
                    self.write_expr(inst);
                }
            }
            ex => {
                let ex = self.expr_to_str(&ex);
                self.pf.code.push(ex)
            }
        }
    }
}

#[derive(Default)]
struct PyFileBuilder {
    funcs: Vec<PyFun>,
}

pub struct Emitter;

impl Emitter {
    pub fn emit(unit: &hir::Unit) -> String {
        // First the functions
        let mut file = PyFileBuilder::default();
        PyFunBuilder::build_entry(unit, &mut file, &unit.expr);
        for fun in &unit.funpool {
            // fun must not be STD
            if matches!(fun.body, hir::FunBody::Std | hir::FunBody::Native) {
                continue;
            }

            PyFunBuilder::build(unit, &mut file, fun);
        }

        // Dump
        let mut outp = Vec::new();
        outp.extend(STD.iter().map(|e| e.to_string()));

        for fun in file.funcs {
            outp.push(format!(
                "def {}({})->{}:",
                fun.name,
                fun.params
                    .into_iter()
                    .map(|e| format!("{}:{}", e.0, e.1))
                    .collect::<Vec<_>>()
                    .join(","),
                fun.ret
            ));

            if fun.code.len() == 0 {
                outp.push(" pass".into());
            }

            for (i, line) in fun.code.iter().enumerate() {
                if i == fun.code.len() - 1 {
                    outp.push(format!(" return {line}"));
                } else {
                    outp.push(format!(" {line}"));
                }
            }
        }

        outp.push("main()".into());

        outp.join("\n")
    }
}
