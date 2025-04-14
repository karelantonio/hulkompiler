//! Parse module: transform a list of tokens into higher-level language structures

use crate::lex::{LexError, Tk};
use std::borrow::Cow;
use thiserror::Error;

/// A binary operator
#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
    Pwr,
}

/// Add two values
#[derive(Debug)]
pub struct BinOpExpr {
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

/// A function call expression
#[derive(Debug)]
pub struct FunCallExpr {
    pub name: String,
    pub args: Vec<Expr>,
}

/// A block, a sequence of expressions between { }
#[derive(Debug)]
pub struct BlockExpr {
    pub exprs: Vec<Expr>,
}

// An expression
#[derive(Debug)]
pub enum Expr {
    Num(f64),
    Id(String),
    Str(String),
    BinOpExpr(BinOpExpr),
    FunCallExpr(FunCallExpr),
    BlockExpr(BlockExpr),
}

/// A function argument
#[derive(Debug)]
pub struct FunArg {
    pub name: String,
    pub ty: Option<String>,
}

/// A function declaration
#[derive(Debug)]
pub struct FunDecl {
    pub name: String,
    pub args: Vec<FunArg>,
    pub ret: Option<String>,
    pub body: Expr,
}

/// A root element
#[derive(Debug)]
pub enum RootElem {
    FunDecl(FunDecl),
    Statement(Expr),
}

/// Errors that may happen while parsing
#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Could not tokenize the data")]
    Lexing(#[from] LexError),

    #[error("Unexpected token {tok:?} at line: {line}, expecting one of: {expected:?} (Near: ...{value}...)")]
    Unexpected {
        tok: Tk, // to make the borrow checker happy :)
        line: usize,
        value: String,
        expected: Cow<'static, [Tk]>,
    },

    #[error("Reached end of file, expecting one of: {expected:?}")]
    EarlyEof { expected: Cow<'static, [Tk]> },

    #[error("Could not parse number: {value}")]
    ParseNum {
        value: String,
        #[source]
        err: std::num::ParseFloatError,
    },
}

/// The parser
pub struct Parser<'a> {
    ptr: usize,
    data: Vec<Tk>,
    lines: Vec<usize>,
    slices: Vec<&'a str>,
}

type PResult<T> = Result<T, ParseError>;

impl<'a> Parser<'a> {
    /// Parse the given string of data
    pub fn parse(data: &str) -> Result<Vec<RootElem>, ParseError> {
        let lex = crate::lex::tokenize_data(data)?;
        let mut parser = Parser {
            ptr: 0,
            data: lex.iter().map(|(_, tk, _)| tk.clone()).collect(),
            lines: lex.iter().map(|(line, _, _)| *line).collect(),
            slices: lex.iter().map(|(_, _, sli)| *sli).collect(),
        };

        let mut res = Vec::new();
        while let Some(roote) = parser.reduce_root_elem()? {
            res.push(roote);
        }

        Ok(res)
    }

    pub fn save(&self) -> usize {
        self.ptr
    }

    pub fn restore(&mut self, ptr: usize) {
        self.ptr = ptr;
    }

    fn take(&mut self) -> &'a str {
        let val = self.slices[self.ptr];
        self.ptr += 1;
        val
    }

    fn remaining(&self) -> &[Tk] {
        &self.data[self.ptr..]
    }

    fn unexpected(&mut self, exp: &'static [Tk]) -> ParseError {
        if let Some(tok) = self.data.get(self.ptr) {
            ParseError::Unexpected {
                line: self.lines[self.ptr],
                expected: exp.into(),
                tok: tok.clone(),
                value: self.slices[self.ptr].into(),
            }
        } else {
            ParseError::EarlyEof {
                expected: exp.into(),
            }
        }
    }

    /// Read a function declaration or an expression
    /// <root-elem> ::= <fun-decl> | <statement> | <EOF>
    fn reduce_root_elem(&mut self) -> PResult<Option<RootElem>> {
        Ok(match self.remaining() {
            [] => return Ok(None),
            [Tk::Function, ..] => Some(RootElem::FunDecl(self.reduce_fun_decl()?)),
            _ => Some(RootElem::Statement(self.reduce_statement()?)),
        })
    }

    /// Reduce a function declaration
    /// <fun-decl> ::= "function" ID '(' <args>... ')' <fun-body>
    fn reduce_fun_decl(&mut self) -> PResult<FunDecl> {
        // Expect function kw
        match self.remaining() {
            [Tk::Function, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::Function])),
        }

        // Expect an identifier
        let name = match self.remaining() {
            [Tk::Id, ..] => self.take().into(),
            _ => return Err(self.unexpected(&[Tk::Id])),
        };

        // Expect an par open: (
        match self.remaining() {
            [Tk::LPar, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::LPar])),
        }

        // Expect arguments
        let args = match self.remaining() {
            [Tk::RPar, ..] => vec![],
            _ => self.reduce_fun_args()?,
        };

        // Expect RPar
        match self.remaining() {
            [Tk::RPar, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::RPar])),
        }

        // Maybe a type
        let ret = match self.remaining() {
            [Tk::Colon, ..] => {
                self.take();

                match self.remaining() {
                    [Tk::Id, ..] => Some(self.take().into()),
                    _ => return Err(self.unexpected(&[Tk::Id])),
                }
            }
            _ => None,
        };

        // Expect either a fat arrow of a code block
        let body = match self.remaining() {
            [Tk::RArrow, ..] => {
                self.take();
                self.reduce_statement()?
            }
            [Tk::LBrac, ..] => self.reduce_statement()?,
            _ => return Err(self.unexpected(&[Tk::RArrow, Tk::LBrac])),
        };

        // Done :)
        Ok(FunDecl {
            name,
            args,
            ret,
            body,
        })
    }

    /// Reduce function args (more than one)
    /// <fun-args> ::= ID [ ':' ID ] [ ',' <fun-args> ]
    fn reduce_fun_args(&mut self) -> PResult<Vec<FunArg>> {
        let mut res = Vec::new();
        self.reduce_fun_args_inner(&mut res)?;
        Ok(res)
    }

    fn reduce_fun_args_inner(&mut self, out: &mut Vec<FunArg>) -> PResult<()> {
        let name = match self.remaining() {
            [Tk::Id, ..] => self.take().into(),
            _ => return Err(self.unexpected(&[Tk::Id, Tk::RPar])),
        };

        // Maybe the type
        let ty = match self.remaining() {
            [Tk::Colon, ..] => {
                self.take();

                match self.remaining() {
                    [Tk::Id, ..] => Some(self.take().into()),
                    _ => return Err(self.unexpected(&[Tk::Id])),
                }
            }
            _ => None,
        };

        let arg = FunArg { name, ty };
        out.push(arg);

        // Check if there is more
        match self.remaining() {
            [Tk::Comma, ..] => {
                self.take();

                self.reduce_fun_args_inner(out)
            }
            _ => Ok(()),
        }
    }

    /// A statement (?) is an expresion ended with a semicolon ;
    /// <statement> ::= <expr> ';'
    fn reduce_statement(&mut self) -> PResult<Expr> {
        // Check if is a conditional or a loop
        /*match self.remaining() {
            [Tk::If]
        }*/

        let was_block = matches!(self.remaining(), [Tk::LBrac, ..]);

        let expr = self.reduce_expr()?;
        match self.remaining() {
            [Tk::Semicolon, ..] => {
                self.take();
                Ok(expr)
            }
            _ => {
                if was_block {
                    Ok(expr)
                } else {
                    Err(self.unexpected(&[Tk::Semicolon]))
                }
            }
        }
    }

    /// The root of the expression parsing
    /// <expr> ::= <ident> '(' <expr>,<expr>... ,? ')'
    ///          | <atom> <binop> <expr>
    ///          | <expr-block>
    fn reduce_expr(&mut self) -> PResult<Expr> {
        // Lookahead
        match self.remaining() {
            [Tk::LBrac, ..] => self.reduce_expr_block(),
            _ => self.reduce_expr_sum(),
        }
    }

    /// Reduce an expression block
    /// <exp-block> ::= '{' <stmt> ... '}'
    fn reduce_expr_block(&mut self) -> PResult<Expr> {
        let _ = match self.remaining() {
            [Tk::LBrac, ..] => self.take(),
            _ => return Err(self.unexpected(&[Tk::LBrac])),
        };

        // Expect statements
        let mut stm = Vec::new();
        while !matches!(self.remaining(), [Tk::RBrac, ..]) {
            stm.push(self.reduce_statement()?);
        }
        self.take();

        Ok(Expr::BlockExpr(BlockExpr { exprs: stm }))
    }

    /// Reduce a sum of values
    /// <sum> :: <mult> [ ('+'|'-') <mult> ]
    fn reduce_expr_sum(&mut self) -> PResult<Expr> {
        let lhs = self.reduce_expr_mult()?;

        Ok(match self.remaining() {
            [Tk::Add, ..] => {
                self.take();
                Expr::BinOpExpr(BinOpExpr {
                    op: BinOp::Add,
                    left: lhs.into(),
                    right: self.reduce_expr_sum()?.into(),
                })
            }
            [Tk::Minus, ..] => {
                self.take();
                Expr::BinOpExpr(BinOpExpr {
                    op: BinOp::Sub,
                    left: lhs.into(),
                    right: self.reduce_expr_sum()?.into(),
                })
            }
            _ => lhs,
        })
    }

    /// Reduce a posible multiplication
    /// <mult> ::= <factor-or-power> [ ('*'|'/') <mult> ] ...
    fn reduce_expr_mult(&mut self) -> PResult<Expr> {
        let b = self.reduce_expr_factor_or_power()?;

        Ok(match self.remaining() {
            [Tk::Star, ..] => {
                self.take();
                Expr::BinOpExpr(BinOpExpr {
                    op: BinOp::Mult,
                    left: b.into(),
                    right: self.reduce_expr_mult()?.into(),
                })
            }
            [Tk::Slash, ..] => {
                self.take();
                Expr::BinOpExpr(BinOpExpr {
                    op: BinOp::Div,
                    left: b.into(),
                    right: self.reduce_expr_mult()?.into(),
                })
            }
            _ => b,
        })
    }

    /// Reduce an power expresion or just an atom
    /// <factor-or-power> ::= <atom> [ ^ <atom> ]
    fn reduce_expr_factor_or_power(&mut self) -> PResult<Expr> {
        let b = self.reduce_expr_atom()?;
        let _ = match self.remaining() {
            [Tk::Power, ..] => self.take(),
            _ => return Ok(b),
        };
        let e = self.reduce_expr_atom()?;

        Ok(Expr::BinOpExpr(BinOpExpr {
            op: BinOp::Pwr,
            left: b.into(),
            right: e.into(),
        }))
    }

    /// Reduce an atom
    /// <atom> ::= <number> | <ident> | <string> | '(' <expr> ')' | <fn-call>
    fn reduce_expr_atom(&mut self) -> PResult<Expr> {
        match self.remaining() {
            [Tk::Num, ..] => {
                // Found number, advance and return gracefully
                let sli = self.take();
                Ok(Expr::Num(sli.parse().map_err(|e| {
                    ParseError::ParseNum {
                        value: sli.into(),
                        err: e,
                    }
                })?))
            }
            [Tk::Str, ..] => {
                let stri = self.take();
                Ok(Expr::Str(stri[1..stri.len() - 1].into()))
            }
            [Tk::Id, Tk::LPar, ..] => self.reduce_expr_fn_call(),
            [Tk::Id, ..] => {
                // Pop the identifier
                let id = self.take();
                Ok(Expr::Id(id.into()))
            }
            [Tk::LPar, ..] => {
                self.take();
                let sub = self.reduce_expr()?;
                // Expect )
                let _ = match self.remaining() {
                    [Tk::RPar, ..] => self.take(),
                    _ => return Err(self.unexpected(&[Tk::RPar])),
                };

                Ok(sub)
            }
            _ => Err(self.unexpected(&[Tk::Num, Tk::Str, Tk::Id, Tk::LPar])),
        }
    }

    /// Reduce a function call
    fn reduce_expr_fn_call(&mut self) -> PResult<Expr> {
        let id = match self.remaining() {
            [Tk::Id, ..] => self.take(),
            _ => return Err(self.unexpected(&[Tk::Id])),
        };

        if !matches!(self.remaining(), [Tk::LPar, ..]) {
            return Err(self.unexpected(&[Tk::LPar]));
        }
        self.take(); // (

        let mut args = Vec::new();

        // If there is a rpar just after (  (i.e: name()  )
        if matches!(self.remaining(), [Tk::RPar, ..]) {
            self.take();
            return Ok(Expr::FunCallExpr(FunCallExpr {
                name: id.into(),
                args: Vec::new(),
            }));
        }

        // Now pop comma-separated exprs
        loop {
            // Expression expected
            args.push(self.reduce_expr()?);

            match self.remaining() {
                [Tk::RPar, ..] => {
                    self.take();
                    break;
                }
                [Tk::Comma, Tk::RPar, ..] => {
                    self.take();
                    self.take();
                    break;
                }
                [Tk::Comma, ..] => {
                    self.take();
                }
                _ => return Err(self.unexpected(&[Tk::RPar, Tk::Comma])),
            }
        }

        Ok(Expr::FunCallExpr(FunCallExpr {
            name: id.into(),
            args,
        }))
    }
}
