//! Transpile HULK to python (3)

use crate::hir;

pub enum Instr {
    Instr(String),
}

pub struct PyFunc {
    name: String,
    args: Vec<String>,
    ret: String,
    code: Vec<Instr>,
}

pub struct PyFile {
    funcs: Vec<PyFunc>,
    glob: Vec<Instr>,
}

const STD: &[&str] = &[
    "# Standard library of HULK",
    "from math import sin, cos, sqrt, exp, log, pi",
    "from random import uniform",
    "def hk_print(arg:object)->object: print(arg)",
    "def hk_sqrt(n:float)->float: return sqrt(n)",
    "def hk_sin(n:float)->float: return sin(n)",
    "def hk_cos(n:float)->float: return cos(n)",
    "def hk_exp(n:float)->float: return exp(n)",
    "def hk_log(b:float,a:float)->float: return log(a, base=b)",
    "def hk_rand()->float: return uniform(0, 1)",
    "hkv_PI = pi",
    "# End of HULK standard library",
];

impl PyFile {
    /// Dump this as a python file
    pub fn dump(&self) -> String {
        let mut res = STD.iter().map(|e| e.to_string()).collect::<Vec<_>>();
        // First the functions
        for fun in self.funcs.iter() {
            self.dump_fun(&fun, &mut res);
        }
        self.dump_instrs(0, false, self.glob.as_slice(), &mut res);

        res.join("\n")
    }

    /// Make the left spacing
    pub fn make_shift(&self, len: usize) -> String {
        let chrs = vec![' ' as u8; len];
        String::from_utf8(chrs).expect("This should be valid utf-8")
    }

    /// Dump a function
    pub fn dump_fun(&self, fun: &PyFunc, lines: &mut Vec<String>) {
        lines.push(format!(
            "def {}({}) -> {}:",
            fun.name,
            fun.args.join(","),
            fun.ret
        ));
        self.dump_instrs(1, true, &fun.code, lines);
    }

    /// Dump multiple instructions
    pub fn dump_instrs(&self, depth: usize, isfun: bool, instrs: &[Instr], out: &mut Vec<String>) {
        let len = instrs.len();
        for (i, instr) in instrs.iter().enumerate() {
            self.dump_instr(depth, isfun && i == len - 1, instr, out);
        }
    }

    /// Dump instruction
    pub fn dump_instr(&self, depth: usize, isret: bool, instr: &Instr, out: &mut Vec<String>) {
        let shift = self.make_shift(depth);
        match instr {
            Instr::Instr(instr) => out.push(format!(
                "{shift}{}{instr}",
                if isret { "return " } else { "" }
            )),
        }
    }
}

pub struct Emitter<'a> {
    unit: &'a hir::Unit,
    fnnum: usize,
}

impl<'a> Emitter<'a> {
    pub fn emit(unit: &'a hir::Unit) -> String {
        Self { unit, fnnum: 0 }.generate()
    }

    fn generate(&mut self) -> String {
        // The output
        let mut file = PyFile {
            funcs: vec![],
            glob: vec![Instr::Instr("pass".into())],
        };

        // The functions
        for fun in self.unit.funpool.iter() {
            self.fun_to_py(&mut file, fun);
        }

        // The global expression
        file.glob = vec![self.expr_to_py(&mut file, &self.unit.expr)];

        file.dump()
    }

    /// Convert our types to python types
    fn ty_to_py(&mut self, ty: &hir::Ty) -> &'static str {
        match ty {
            hir::Ty::Obj => "object",
            hir::Ty::Str => "str",
            hir::Ty::Num => "float",
            hir::Ty::Bool => "bool",
        }
    }

    /// Generate a new function name
    fn make_function_name(&mut self) -> String {
        self.fnnum += 1;
        format!("_{:06}", self.fnnum)
    }

    /// Convert the given constant to a string
    fn const_val_to_str(&self, ct: &hir::Const) -> String {
        match &ct.value {
            hir::ConstValue::Num(v) => format!("{v}"),
            hir::ConstValue::Str(v) => format!("\"{v}\""),
            hir::ConstValue::Bool(v) => if *v { "True" } else { "False" }.into(),
        }
    }

    /// Export a function
    fn fun_to_py(&mut self, outp: &mut PyFile, fun: &hir::Fun) {
        let args = fun
            .args
            .iter()
            .map(|e| format!("{}:{}", e.name, self.ty_to_py(&e.ty)))
            .collect::<Vec<_>>();
        let ret = self.ty_to_py(&fun.ty);

        let code = match &fun.body {
            hir::FunBody::Std => {
                return; // This function is implemented on top of the file
            }
            hir::FunBody::Expr(hir::Expr::Block { insts, .. }) => {
                insts.iter().map(|e| self.expr_to_py(outp, e)).collect()
            }
            hir::FunBody::Expr(e) => vec![self.expr_to_py(outp, &e)],
            e => panic!("Dont know how to proceed (known body type: {:?})", e),
        };

        let fun = PyFunc {
            name: format!("hk_{}", fun.name),
            args,
            ret: ret.into(),
            code,
        };

        outp.funcs.push(fun);
    }

    /// Export a block as a function, return its name
    fn block_as_py_fun(&mut self, outp: &mut PyFile, expr: &hir::Expr) -> String {
        let name = self.make_function_name();

        // Build the code
        let code = match expr {
            hir::Expr::Block { ty: _, insts } => {
                insts.iter().map(|e| self.expr_to_py(outp, e)).collect()
            }
            a => vec![self.expr_to_py(outp, a)],
        };

        // Build a function
        let fun = PyFunc {
            name: name.clone(),
            args: vec![],
            ret: self.ty_to_py(&expr.ty()).into(),
            code,
        };

        outp.funcs.push(fun);

        name
    }

    /// Convert an instruction to a string
    fn instr_to_str(&mut self, inst: &Instr) -> String {
        match inst {
            Instr::Instr(v) => v.clone(),
        }
    }

    /// Convert an expresion to python, without indentation
    fn expr_to_py(&mut self, outp: &mut PyFile, expr: &hir::Expr) -> Instr {
        match expr {
            hir::Expr::Const { cons, ty: _ } => {
                // Just get the value, if it is a string
                let val = self
                    .unit
                    .lookup_const(cons)
                    .expect("Const expected to be inside Unit");
                Instr::Instr(self.const_val_to_str(&val))
            }
            hir::Expr::BinOp {
                op,
                ty: _,
                left,
                right,
            } => {
                let opchar = match op {
                    hir::Op::Add => "+",
                    hir::Op::Sub => "-",
                    hir::Op::Div => "/",
                    hir::Op::Mul => "*",
                    hir::Op::Pow => "**",
                    hir::Op::Cat => "+",
                };

                // Make it work, then make it pretty
                let lty = left.ty();
                let rty = right.ty();
                let left = self.expr_to_py(outp, &left);
                let right = self.expr_to_py(outp, &right);

                let left = self.instr_to_str(&left);
                let right = self.instr_to_str(&right);

                match op {
                    hir::Op::Cat => {
                        let left = if lty == hir::Ty::Str {
                            format!("({left})")
                        } else {
                            format!("str({left})")
                        };

                        let right = if rty == hir::Ty::Str {
                            format!("({right})")
                        } else {
                            format!("str({right})")
                        };

                        Instr::Instr(format!("{left}+{right}"))
                    }
                    _ => Instr::Instr(format!("({left}){opchar}({right})")),
                }
            }
            hir::Expr::Call { fun, ty: _, args } => {
                let fun = self
                    .unit
                    .lookup_fun(fun)
                    .expect("Expected an existing function"); // Its OK, if the function did not
                                                              // exists then why we have a type
                                                              // checker?
                let args: Vec<_> = args
                    .iter()
                    .map(|e| {
                        let arg = self.expr_to_py(outp, e);
                        self.instr_to_str(&arg)
                    })
                    .collect();
                Instr::Instr(format!("hk_{}({})", fun.name, args.join(",")))
            }
            hir::Expr::Block { ty: _, insts: _ } => {
                Instr::Instr(format!("{}()", self.block_as_py_fun(outp, expr)))
            }
            hir::Expr::ImplicitCast { ty: _, expr } => {
                // In python is not required to do any black magic, just ol' polimorphism
                self.expr_to_py(outp, expr)
            }
            hir::Expr::VarRead { ty: _, var } => {
                let var = self
                    .unit
                    .lookup_var(var)
                    .expect("Expect that variable to exist");
                Instr::Instr(format!("hkv_{}", var.name))
            }
            hir::Expr::UnaryOp { ty: _, op, expr } => match op {
                hir::UOp::Neg => {
                    let expr = self.expr_to_py(outp, expr);

                    Instr::Instr(format!("(-({}))", self.instr_to_str(&expr)))
                }
            },
        }
    }
}
