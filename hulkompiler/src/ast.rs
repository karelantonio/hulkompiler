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
    pub fn parse(data: &str) -> Result<Expr, ParseError> {
        let lex = crate::lex::tokenize_data(data)?;
        let mut parser = Parser {
            ptr: 0,
            data: lex.iter().map(|(_, tk, _)| tk.clone()).collect(),
            lines: lex.iter().map(|(line, _, _)| *line).collect(),
            slices: lex.iter().map(|(_, _, sli)| *sli).collect(),
        };

        Ok(parser.reduce_expr()?)
    }

    fn save(&self) -> usize {
        self.ptr
    }

    fn restore(&mut self, ptr: usize) {
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

    fn slice_values(&self) -> &[&'a str] {
        &self.slices[self.ptr..]
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

    fn map_operator(&self, op: Tk) -> Option<BinOp> {
        Some(match op {
            Tk::Add => BinOp::Add,
            Tk::Minus => BinOp::Sub,
            Tk::Star => BinOp::Mult,
            Tk::Slash => BinOp::Div,
            _ => return None,
        })
    }

    /// A statement (?) is an expresion ended with a semicolon ;
    /// <statement> ::= <expr> ';'
    fn reduce_statement(&mut self) -> PResult<Expr> {
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
            [Tk::Id, Tk::LPar, ..] => self.reduce_expr_fn_call(),
            [Tk::LBrac, ..] => self.reduce_expr_block(),
            [_, op, ..] if self.map_operator(op.clone()).is_some() => self.reduce_expr_binop(),
            _ => self.reduce_expr_atom(),
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

    /// Reduce a binary operation
    fn reduce_expr_binop(&mut self) -> PResult<Expr> {
        let lhs = self.reduce_expr_atom()?;

        let op = if let [tok, ..] = self.remaining() {
            let Some(op) = self.map_operator(tok.clone()) else {
                return Err(self.unexpected(&[Tk::Add, Tk::Minus, Tk::Star, Tk::Slash]));
            };
            self.take();
            op
        } else {
            return Err(self.unexpected(&[Tk::Add, Tk::Minus, Tk::Star, Tk::Slash]));
        };

        let rhs = self.reduce_expr()?;

        Ok(Expr::BinOpExpr(BinOpExpr {
            op,
            left: lhs.into(),
            right: rhs.into(),
        }))
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

    /// Reduce an atom
    /// <atom> ::= <number> | <ident>
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
            [Tk::Id, ..] => {
                // Pop the identifier
                let id = self.take();
                Ok(Expr::Id(id.into()))
            }
            _ => Err(self.unexpected(&[Tk::Num])),
        }
    }
}
