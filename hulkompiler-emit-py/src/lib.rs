//! Transpile HULK to python (3)

use hulkompiler_hir as hir;
use std::collections::BTreeMap;

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
fn ty_to_str(ty: &hir::ty::Ty) -> &str {
    match ty {
        hir::ty::Ty::Obj => "object",
        hir::ty::Ty::Str => "str",
        hir::ty::Ty::Num => "float",
        hir::ty::Ty::Bool => "bool",
    }
}

struct PyFun {
    name: String,
    ret: String,
    params: Vec<(String, String)>,
    varmap: BTreeMap<usize, usize>,
    code: Vec<String>,
}

struct PyFunBuilder<'a> {
    pf: PyFun,
    unit: &'a hir::unit::Unit,
    file: &'a mut PyFileBuilder,
}

impl<'a> PyFunBuilder<'a> {
    fn lookup_var_read(&mut self, vid: &hir::expr::VarId) -> String {
        let var = self.unit.lookup_var(vid);

        match var {
            Some(hir::expr::Var {
                name,
                kind: hir::expr::VarKind::Global,
                ..
            }) => format!("hkv_{name}"),
            _ => format!("locals[{}]", vid.id()),
        }
    }

    fn lookup_var_set(&mut self, vid: &hir::expr::VarId, val: String) -> String {
        let var = self.unit.lookup_var(vid);
        match var {
            Some(hir::expr::Var {
                name,
                kind: hir::expr::VarKind::Global,
                ..
            }) => format!("hkv_{name}=({val})"),
            _ => format!("locals.__setitem__({},{})", vid.id(), val),
        }
    }

    fn push_var(&mut self, vid: &hir::expr::VarId) -> String {
        format!("locals[{}]", vid.id())
    }

    /// The program entry point
    fn build_entry(unit: &'a hir::unit::Unit, file: &'a mut PyFileBuilder, expr: &hir::expr::Expr) {
        let pf = PyFun {
            name: "main".into(),
            ret: "None".into(),
            params: vec![],
            varmap: Default::default(),
            code: vec![],
        };
        let mut slf = Self { pf, unit, file };
        slf.pf.code.push("locals=dict()".into());
        slf.write_expr(expr);
        slf.file.funcs.push(slf.pf);
    }

    /// Build a function
    fn build(unit: &'a hir::unit::Unit, file: &'a mut PyFileBuilder, fun: &hir::expr::Fun) {
        let pf = PyFun {
            name: format!("hk_{}", fun.name),
            ret: ty_to_str(&fun.ty).into(),
            params: fun
                .args
                .iter()
                .map(|arg| (format!("p_{}", arg.name), ty_to_str(&arg.ty).into()))
                .collect(),
            varmap: Default::default(),
            code: vec![],
        };

        Self::build_pyfun(unit, file, pf, &fun.body, &fun.args)
    }

    /// Build a pyfunction
    fn build_pyfun(
        unit: &'a hir::unit::Unit,
        file: &'a mut PyFileBuilder,
        pf: PyFun,
        body: &hir::expr::FunBody,
        params: &[hir::expr::FunArg],
    ) {
        let mut slf = Self { pf, unit, file };

        // add the args
        slf.pf.code.push("locals=dict()".into());
        let mut bld = Vec::new();
        for arg in params {
            bld.push(format!("{}=p_{}", slf.push_var(&arg.id), arg.name));
        }
        slf.pf.code.push(bld.join(";"));

        match body {
            hir::expr::FunBody::Expr(e) => slf.write_expr(&e),
            hir::expr::FunBody::Std => slf.pf.code.push("pass #Defined in STD".into()),
            hir::expr::FunBody::Native => slf.pf.code.push("pass #Native".into()),
        }

        slf.file.funcs.push(slf.pf);
    }

    fn expr_to_str(&mut self, expr: &hir::expr::Expr) -> String {
        match expr {
            // Ignore, python makes it easy
            hir::expr::Expr::ImplicitCast { ty: _, expr } => self.expr_to_str(expr),

            // A constant
            hir::expr::Expr::Const { cons, ty: _ } => {
                // lookup the const in the unit:
                let cons = self
                    .unit
                    .lookup_const(&cons)
                    .expect("Constant did not exist, program is in a bad state.");
                match &cons.value {
                    hir::expr::ConstValue::Num(nu) => format!("{nu}"),
                    hir::expr::ConstValue::Bool(bo) => if *bo { "True" } else { "False" }.into(),
                    hir::expr::ConstValue::Str(s) => format!("\"{s}\""),
                }
            }

            // Access a variable
            hir::expr::Expr::VarRead { ty: _, var } => self.lookup_var_read(var),

            // A function call
            hir::expr::Expr::Call { fun, ty: _, args } => {
                // First lookup
                let fun = self
                    .unit
                    .lookup_fun(&fun)
                    .expect("Function did not exist, program is in a bad state");
                let args: Vec<_> = args.iter().map(|a| self.expr_to_str(a)).collect();

                format!("hk_{}({})", fun.name, args.join(","))
            }

            // An unary expression
            hir::expr::Expr::UnaryOp { ty: _, op, expr } => {
                let opch = match op {
                    hir::ops::UOp::Neg => "-",
                    hir::ops::UOp::Not => "not",
                };
                let expr = self.expr_to_str(expr);

                format!("{opch}({expr})")
            }

            // A binary expression (Concat)
            hir::expr::Expr::BinOp {
                op: hir::ops::Op::Cat,
                ty: _,
                left,
                right,
            } => {
                let lrepr = self.expr_to_str(left);
                let rrepr = self.expr_to_str(right);
                let left = if left.ty() != hir::ty::Ty::Str {
                    format!("str({lrepr})")
                } else {
                    format!("({lrepr})")
                };
                let right = if right.ty() != hir::ty::Ty::Str {
                    format!("str({rrepr})")
                } else {
                    format!("({rrepr})")
                };

                format!("{left}+{right}")
            }

            // A binary expression
            hir::expr::Expr::BinOp {
                op,
                ty: _,
                left,
                right,
            } => {
                let lrepr = self.expr_to_str(left);
                let rrepr = self.expr_to_str(right);
                let opch = match op {
                    hir::ops::Op::Add => "+",
                    hir::ops::Op::Sub => "-",
                    hir::ops::Op::Mul => "*",
                    hir::ops::Op::Div => "/",
                    hir::ops::Op::Pow => "**",
                    hir::ops::Op::Le => "<=",
                    hir::ops::Op::Lt => "<",
                    hir::ops::Op::Ge => ">=",
                    hir::ops::Op::Gt => ">",
                    hir::ops::Op::Eq => "==",
                    hir::ops::Op::Neq => "!=",
                    hir::ops::Op::And => "and",
                    hir::ops::Op::Or => "or",
                    hir::ops::Op::Cat => unreachable!(),
                };

                format!("({lrepr}){opch}({rrepr})")
            }

            // A group of expressions
            hir::expr::Expr::Block { ty: _, insts } => {
                let res = insts
                    .iter()
                    .map(|e| self.expr_to_str(e))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("[{res}][-1]")
            }

            // A variable declaration
            hir::expr::Expr::VarDecl {
                ty: _,
                var,
                expr,
                scope,
            } => {
                let assi = self.expr_to_str(expr);
                let sco = self.expr_to_str(scope);
                let vset = self.lookup_var_set(var, assi);

                format!("[{vset},{sco}][1]")
            }

            // A conditional expression
            hir::expr::Expr::Branch {
                ty: _,
                cond,
                ontrue,
                onfalse,
            } => {
                let cond = self.expr_to_str(cond);
                let ontrue = self.expr_to_str(ontrue);
                let onfalse = self.expr_to_str(onfalse);

                format!("({ontrue})if({cond})else({onfalse})")
            }

            // A loop structure
            hir::expr::Expr::Loop { ty: _, cond, body } => {
                let cond = self.expr_to_str(cond);
                let body = self.expr_to_str(body);
                format!("whileinstr(lambda:{cond},lambda:{body})")
            }

            // A reassignment (destructive assignment)
            hir::expr::Expr::Reassign { ty: _, var, expr } => {
                let body = self.expr_to_str(expr);
                let assi = self.lookup_var_set(var, body);
                let get = self.lookup_var_read(var);

                format!("[{assi},{get}][1]")
            }
        }
    }

    fn write_expr(&mut self, expr: &hir::expr::Expr) {
        // The root element of the function
        let res = self.expr_to_str(expr);
        self.pf.code.push(res);
    }
}

#[derive(Default)]
struct PyFileBuilder {
    funcs: Vec<PyFun>,
}

pub struct Emitter;

impl Emitter {
    pub fn emit(unit: &hir::unit::Unit) -> String {
        // First the functions
        let mut file = PyFileBuilder::default();
        PyFunBuilder::build_entry(unit, &mut file, &unit.expr);
        for fun in &unit.funpool {
            // fun must not be STD
            if matches!(fun.body, hir::expr::FunBody::Std | hir::expr::FunBody::Native) {
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
