//! Parse module: transform a list of tokens into higher-level language structures

use crate::lex::{Addr, LexError, Tk};
use std::{borrow::Cow, rc::Rc};
use thiserror::Error;

/// Location
#[derive(Debug, Clone)]
pub struct Loc {
    pub start: Addr,
    pub end: Addr,
    pub content: Rc<[String]>,
}

/// A binary operator
#[derive(Debug)]
pub enum BinOp {
    Cat,

    // Arithmetic
    Add,
    Sub,
    Mult,
    Div,
    Pwr,

    // Comparison
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,

    // Logic
    And,
    Or,
}

/// Add two values
#[derive(Debug)]
pub struct BinOpExpr {
    pub loc: Loc,
    pub op: BinOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

/// An unary operator
#[derive(Debug)]
pub enum UnaryOp {
    Neg, // -2
    Not, // !true
}

/// An unary operator expression
#[derive(Debug)]
pub struct UnaryOpExpr {
    pub loc: Loc,
    pub op: UnaryOp,
    pub expr: Box<Expr>,
}

/// A function call expression
#[derive(Debug)]
pub struct FunCallExpr {
    pub loc: Loc,
    pub name: String,
    pub args: Vec<Expr>,
}

/// A block, a sequence of expressions between { }
#[derive(Debug)]
pub struct BlockExpr {
    pub loc: Loc,
    pub exprs: Vec<Expr>,
}

/// A Variable declaration
#[derive(Debug)]
pub struct VarDeclExpr {
    pub loc: Loc,
    pub name: String,
    pub declty: Option<String>,
    pub expr: Box<Expr>,
    pub scope: Box<Expr>,
}

// A conditional expression
#[derive(Debug)]
pub struct IfExpr {
    pub loc: Loc,
    pub cond: Box<Expr>,
    pub ifarm: Box<Expr>,
    pub elsearm: Box<Expr>,
}

/// A while expression
#[derive(Debug)]
pub struct WhileExpr {
    pub loc: Loc,
    pub cond: Box<Expr>,
    pub body: Box<Expr>,
}

/// A reassign (destructive assignment)
#[derive(Debug)]
pub struct Reassign {
    pub loc: Loc,
    pub name: String,
    pub expr: Box<Expr>,
}

// An expression
#[derive(Debug)]
pub enum Expr {
    Num(Loc, f64),
    Id(Loc, String),
    Str(Loc, String),
    Boolean(Loc, bool),
    BinOpExpr(BinOpExpr),
    UnaryOpExpr(UnaryOpExpr),
    FunCallExpr(FunCallExpr),
    BlockExpr(BlockExpr),
    VarDeclExpr(VarDeclExpr),
    IfExpr(IfExpr),
    WhileExpr(WhileExpr),
    Reassign(Reassign),
}

impl Expr {
    pub fn loc(&self) -> &Loc {
        match self {
            Expr::Num(loc, _) => loc,
            Expr::Id(loc, _) => loc,
            Expr::Str(loc, _) => loc,
            Expr::Boolean(loc, _) => loc,
            Expr::BinOpExpr(BinOpExpr { loc, .. }) => loc,
            Expr::FunCallExpr(FunCallExpr { loc, .. }) => loc,
            Expr::BlockExpr(BlockExpr { loc, .. }) => loc,
            Expr::UnaryOpExpr(UnaryOpExpr { loc, .. }) => loc,
            Expr::VarDeclExpr(VarDeclExpr { loc, .. }) => loc,
            Expr::IfExpr(IfExpr { loc, .. }) => loc,
            Expr::WhileExpr(WhileExpr { loc, .. }) => loc,
            Expr::Reassign(Reassign { loc, .. }) => loc,
        }
    }
}

/// A function argument
#[derive(Debug)]
pub struct FunArg {
    pub loc: Loc,
    pub name: String,
    pub ty: Option<String>,
}

/// A function declaration
#[derive(Debug)]
pub struct FunDecl {
    pub loc: Loc,
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

impl RootElem {
    pub fn loc(&self) -> &Loc {
        match self {
            RootElem::FunDecl(FunDecl { loc, .. }) => loc,
            RootElem::Statement(stm) => stm.loc(),
        }
    }
}

/// Errors that may happen while parsing
#[derive(Debug, Error)]
pub enum ParseError {
    #[error("Could not tokenize the data")]
    Lexing(#[from] LexError),

    #[error("Unexpected token {} ({tok:?}), expecting one of: {}", tok.to_string(), expected.into_iter().map(|e| e.to_string()).collect::<Vec<_>>().join(", "))]
    Unexpected {
        #[source]
        addr: crate::sourcehint::LocError,
        tok: Tk, // to make the borrow checker happy :)
        value: String,
        expected: Vec<Tk>,
    },

    #[error("Reached end of file, expecting one of: {expected:?}")]
    EarlyEof { expected: Cow<'static, [Tk]> },

    #[error("Could not parse number {value}: {err}")]
    ParseNum {
        #[source]
        loc: crate::sourcehint::LocError,
        err: std::num::ParseFloatError,
        value: String,
    },
}

/// The parser
pub struct Parser<'a> {
    ptr: usize,
    data: Vec<Tk>,
    start: Vec<Addr>,
    end: Vec<Addr>,
    slices: Vec<&'a str>,
    text: Rc<[String]>, /* Lines */
}

type PResult<T> = Result<T, ParseError>;

impl<'a> Parser<'a> {
    /// Parse the given string of data
    pub fn parse(data: &str) -> Result<Vec<RootElem>, ParseError> {
        let lex = crate::lex::tokenize_data(data)?;
        let lines = data.split('\n').map(|e| e.to_string()).collect::<Vec<_>>();
        let mut parser = Parser {
            ptr: 0,
            data: lex.iter().map(|e| e.tk.clone()).collect(),
            start: lex.iter().map(|e| e.start.clone()).collect(),
            end: lex.iter().map(|e| e.end.clone()).collect(),
            slices: lex.iter().map(|e| e.slice).collect(),
            text: lines.into(),
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

    fn addr_start(&self) -> &Addr {
        &self.start[self.ptr]
    }

    fn addr_end(&self) -> &Addr {
        &self.end[self.ptr]
    }

    fn prev_addr_end(&self) -> &Addr {
        &self.end[self.ptr - 1] // Ok to panic, should alread have parsed some token
    }

    fn make_loc_error(&self, start: &Addr, end: &Addr) -> crate::sourcehint::LocError {
        crate::sourcehint::make(&self.text, start, end)
    }

    fn remaining(&self) -> &[Tk] {
        &self.data[self.ptr..]
    }

    fn unexpected(&mut self, exp: &'static [Tk]) -> ParseError {
        if let Some(tok) = self.data.get(self.ptr) {
            let (start, end) = (self.start[self.ptr].clone(), self.end[self.ptr].clone());
            ParseError::Unexpected {
                addr: crate::sourcehint::make(&self.text, &start, &end),
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
        // Should we only highlight the function declaration, not the body?
        let loc_start = self.addr_start().clone();

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
        let loc_end = self.prev_addr_end().clone();
        let body = match self.remaining() {
            [Tk::RArrow, ..] => {
                self.take();
                self.reduce_statement()?
            }
            [Tk::LBrac, ..] => self.reduce_statement()?,
            _ => return Err(self.unexpected(&[Tk::RArrow, Tk::LBrac])),
        };

        // Done :)
        let loc = Loc {
            content: self.text.clone(),
            start: loc_start,
            end: loc_end,
        };
        Ok(FunDecl {
            loc,
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
        let loc_start = self.addr_start().clone();
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

        // the location
        let loc = Loc {
            content: self.text.clone(),
            start: loc_start,
            end: self.prev_addr_end().clone(),
        };

        let arg = FunArg { loc, name, ty };
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
        let expr = self.reduce_expr()?;
        match self.remaining() {
            [Tk::Semicolon, ..] => {
                self.take();
                Ok(expr)
            }
            _ => {
                let was_block = self
                    .data
                    .get(self.ptr - 1)
                    .map(|e| matches!(e, Tk::RBrac))
                    .unwrap_or(false);

                if was_block {
                    Ok(expr)
                } else {
                    Err(self.unexpected(&[Tk::Semicolon]))
                }
            }
        }
    }

    /// The root of the expression parsing
    /// <expr> ::= <cat>
    fn reduce_expr(&mut self) -> PResult<Expr> {
        // Lookahead
        self.reduce_expr_log_or()
    }

    /// Reduce an expression block
    /// <exp-block> ::= '{' <stmt> ... '}'
    fn reduce_expr_block(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();

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

        let loc = Loc {
            content: self.text.clone(),
            start: loc_start,
            end: self.prev_addr_end().clone(),
        };
        Ok(Expr::BlockExpr(BlockExpr { loc, exprs: stm }))
    }

    /// Reduce a logical OR (disjunction)
    /// <or> ::= <and> [ | <or> ]
    fn reduce_expr_log_or(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();

        let lhs = self.reduce_expr_log_and()?;

        match self.remaining() {
            [Tk::Pipe, ..] => {
                self.take();
            }
            _ => return Ok(lhs),
        }

        let rhs = self.reduce_expr_log_or()?;

        let loc = Loc {
            start: loc_start,
            end: self.prev_addr_end().clone(),
            content: self.text.clone(),
        };
        Ok(Expr::BinOpExpr(BinOpExpr {
            loc,
            op: BinOp::Or,
            left: Box::new(lhs),
            right: Box::new(rhs),
        }))
    }

    /// Reduce a logical AND (conjunction)
    /// <and> ::= <neg> [ & <and> ]
    fn reduce_expr_log_and(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();

        let lhs = self.reduce_expr_log_neg()?;

        match self.remaining() {
            [Tk::Amp, ..] => {
                self.take();
            }
            _ => return Ok(lhs),
        }

        let rhs = self.reduce_expr_log_and()?;

        let loc = Loc {
            start: loc_start,
            end: self.prev_addr_end().clone(),
            content: self.text.clone(),
        };

        Ok(Expr::BinOpExpr(BinOpExpr {
            loc,
            op: BinOp::And,
            left: Box::new(lhs),
            right: Box::new(rhs),
        }))
    }

    /// Reduce a logical negation
    /// <neg> ::= [ ! ] <neg>
    fn reduce_expr_log_neg(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();

        match self.remaining() {
            [Tk::Excl, ..] => {
                self.take();
            }
            _ => return self.reduce_expr_comp(),
        }

        let val = self.reduce_expr_log_neg()?;

        let loc = Loc {
            start: loc_start,
            end: self.prev_addr_end().clone(),
            content: self.text.clone(),
        };

        Ok(Expr::UnaryOpExpr(UnaryOpExpr {
            loc,
            op: UnaryOp::Not,
            expr: Box::new(val),
        }))
    }

    /// Reduce a comparison
    /// <comp> ::= <cat> [ <= | >= | < | > | == | != ] <cat>
    fn reduce_expr_comp(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();
        let lhs = self.reduce_expr_cat()?;
        let op = match self.remaining() {
            [Tk::Le, ..] => BinOp::Le,
            [Tk::Ge, ..] => BinOp::Ge,
            [Tk::Lt, ..] => BinOp::Lt,
            [Tk::Gt, ..] => BinOp::Gt,
            [Tk::Eq, ..] => BinOp::Eq,
            [Tk::Neq, ..] => BinOp::Neq,
            _ => return Ok(lhs),
        };
        self.take();

        let rhs = self.reduce_expr_cat()?;
        let loc = Loc {
            start: loc_start,
            end: self.prev_addr_end().clone(),
            content: self.text.clone(),
        };

        Ok(Expr::BinOpExpr(BinOpExpr {
            loc,
            op,
            left: Box::new(lhs),
            right: Box::new(rhs),
        }))
    }

    /// Reduce a concatenation
    /// <cat> ::= <sum> [ '@' <cat> ]
    fn reduce_expr_cat(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();
        let left = self.reduce_expr_sum()?;

        let right = match self.remaining() {
            [Tk::Cat, ..] => {
                self.take(); // The @
                self.reduce_expr_cat()?
            }
            _ => return Ok(left),
        };

        let loc = Loc {
            start: loc_start,
            end: self.prev_addr_end().clone(),
            content: self.text.clone(),
        };

        Ok(Expr::BinOpExpr(BinOpExpr {
            loc,
            op: BinOp::Cat,
            left: Box::new(left),
            right: Box::new(right),
        }))
    }

    /// Reduce a sum of values
    /// <sum> ::= <mult> [ ('+'|'-') <mult> ]
    fn reduce_expr_sum(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();
        let lhs = self.reduce_expr_mult()?;

        Ok(match self.remaining() {
            [Tk::Add, ..] => {
                self.take();
                let loc = Loc {
                    content: self.text.clone(),
                    start: loc_start,
                    end: self.prev_addr_end().clone(),
                };
                Expr::BinOpExpr(BinOpExpr {
                    loc,
                    op: BinOp::Add,
                    left: lhs.into(),
                    right: self.reduce_expr_sum()?.into(),
                })
            }
            [Tk::Minus, ..] => {
                self.take();
                let loc = Loc {
                    content: self.text.clone(),
                    start: loc_start,
                    end: self.prev_addr_end().clone(),
                };
                Expr::BinOpExpr(BinOpExpr {
                    loc,
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
        let loc_start = self.addr_start().clone();
        let fst = self.reduce_expr_neg_factor_or_power()?;

        Ok(match self.remaining() {
            [Tk::Star, ..] => {
                self.take();
                let loc = Loc {
                    content: self.text.clone(),
                    start: loc_start,
                    end: self.prev_addr_end().clone(),
                };
                Expr::BinOpExpr(BinOpExpr {
                    loc,
                    op: BinOp::Mult,
                    left: fst.into(),
                    right: self.reduce_expr_mult()?.into(),
                })
            }
            [Tk::Slash, ..] => {
                self.take();
                let loc = Loc {
                    content: self.text.clone(),
                    start: loc_start,
                    end: self.prev_addr_end().clone(),
                };
                Expr::BinOpExpr(BinOpExpr {
                    loc,
                    op: BinOp::Div,
                    left: fst.into(),
                    right: self.reduce_expr_mult()?.into(),
                })
            }
            _ => fst,
        })
    }

    /// Reduce a possibly negated value
    /// <neg-factor-or-power> ::= [ '-' ] <factor-or-power>
    fn reduce_expr_neg_factor_or_power(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();
        match self.remaining() {
            [Tk::Minus, ..] => {
                self.take();

                let expr = self.reduce_expr_factor_or_power()?;
                let loc = Loc {
                    start: loc_start,
                    end: self.prev_addr_end().clone(),
                    content: self.text.clone(),
                };

                Ok(Expr::UnaryOpExpr(UnaryOpExpr {
                    loc,
                    op: UnaryOp::Neg,
                    expr: Box::new(expr),
                }))
            }
            _ => self.reduce_expr_factor_or_power(),
        }
    }

    /// Reduce an power expresion or just an atom
    /// <factor-or-power> ::= <atom> [ ^ <atom> ]
    fn reduce_expr_factor_or_power(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();

        let base = self.reduce_expr_atom()?;
        let _ = match self.remaining() {
            [Tk::Power, ..] => self.take(),
            _ => return Ok(base),
        };
        let exp = self.reduce_expr_atom()?;

        let loc = Loc {
            content: self.text.clone(),
            start: loc_start,
            end: self.prev_addr_end().clone(),
        };
        Ok(Expr::BinOpExpr(BinOpExpr {
            loc,
            op: BinOp::Pwr,
            left: base.into(),
            right: exp.into(),
        }))
    }

    /// Reduce an atom
    /// <atom> ::= <number> | <ident> | <string> | '(' <expr> ')' | <fn-call> | true | false
    fn reduce_expr_atom(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();
        match self.remaining() {
            // Boolean true
            [Tk::True, ..] => {
                let loc_end = self.addr_end();
                let loc = Loc {
                    start: loc_start,
                    end: loc_end.clone(),
                    content: self.text.clone(),
                };
                self.take();
                Ok(Expr::Boolean(loc, true))
            }

            // Boolean false
            [Tk::False, ..] => {
                let loc_end = self.addr_end();
                let loc = Loc {
                    start: loc_start,
                    end: loc_end.clone(),
                    content: self.text.clone(),
                };
                self.take();
                Ok(Expr::Boolean(loc, false))
            }

            // Constant number
            [Tk::Num, ..] => {
                // Found number, advance and return gracefully
                let loc_end = self.addr_end().clone();
                let sli = self.take();
                let loc = Loc {
                    content: self.text.clone(),
                    start: loc_start,
                    end: loc_end,
                };

                Ok(Expr::Num(
                    loc,
                    sli.parse().map_err(|e| ParseError::ParseNum {
                        value: sli.into(),
                        err: e,
                        loc: self.make_loc_error(&loc_start, &loc_end),
                    })?,
                ))
            }

            // Constant string (should escape characters later
            [Tk::Str, ..] => {
                let loc = Loc {
                    content: self.text.clone(),
                    start: loc_start,
                    end: self.addr_end().clone(),
                };
                let stri = self.take();
                Ok(Expr::Str(loc, stri[1..stri.len() - 1].into()))
            }

            // A function call
            [Tk::Id, Tk::LPar, ..] => self.reduce_expr_fn_call(),

            // A reassign
            [Tk::Id, Tk::Reassign, ..] => self.reduce_expr_reassign(),

            // A variable
            [Tk::Id, ..] => {
                // Pop the identifier
                let loc = Loc {
                    content: self.text.clone(),
                    start: loc_start,
                    end: self.addr_end().clone(),
                };
                let id = self.take();
                Ok(Expr::Id(loc, id.into()))
            }

            // A parenthesized expresion
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

            // An expression block
            [Tk::LBrac, ..] => self.reduce_expr_block(),

            // A variable declaration
            [Tk::Let, ..] => self.reduce_expr_var_decl(),

            // A conditional
            [Tk::If, ..] => self.reduce_expr_if(),

            // A while loop
            [Tk::While, ..] => self.reduce_expr_while(),

            _ => Err(self.unexpected(&[Tk::Num, Tk::Str, Tk::Id, Tk::LPar])),
        }
    }

    /// Reduce a reassign (destructive assignment)
    /// <reassign> ::= ID ':=' <expr>
    fn reduce_expr_reassign(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();
        let name = match self.remaining() {
            [Tk::Id, ..] => self.take().to_string(),
            _ => return Err(self.unexpected(&[Tk::Id])),
        };

        // :=
        match self.remaining() {
            [Tk::Reassign, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::Reassign])),
        }

        // Expr
        let expr = self.reduce_expr()?;

        let loc = Loc {
            start: loc_start,
            end: self.prev_addr_end().clone(),
            content: self.text.clone(),
        };

        Ok(Expr::Reassign(Reassign {
            loc,
            name: name,
            expr: Box::new(expr),
        }))
    }

    /// Reduce a while loop
    /// <while> ::= WHILE ( <expr> ) <expr>
    fn reduce_expr_while(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();

        // while
        match self.remaining() {
            [Tk::While, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::While])),
        }

        // (
        match self.remaining() {
            [Tk::LPar, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::LPar])),
        }

        // Cond
        let cond = self.reduce_expr()?;

        // )
        match self.remaining() {
            [Tk::RPar, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::RPar])),
        }

        // Body
        let body = self.reduce_expr()?;

        // Done :)
        let loc = Loc {
            start: loc_start,
            end: self.prev_addr_end().clone(),
            content: self.text.clone(),
        };
        Ok(Expr::WhileExpr(WhileExpr {
            loc,
            cond: Box::new(cond),
            body: Box::new(body),
        }))
    }

    /// Reduce an IF ELIF ELSE conditional
    /// <if> ::= IF ( <expr> ) <expr> <elif-chain>
    fn reduce_expr_if(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();

        match self.remaining() {
            [Tk::If, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::If])),
        }

        match self.remaining() {
            [Tk::LPar, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::LPar])),
        }

        let cond = self.reduce_expr()?;

        match self.remaining() {
            [Tk::RPar, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::RPar])),
        }

        let ifbody = self.reduce_expr()?;

        let elsebody = self.reduce_expr_elif_chain()?;

        let loc = Loc {
            start: loc_start,
            end: self.prev_addr_end().clone(),
            content: self.text.clone(),
        };

        Ok(Expr::IfExpr(IfExpr {
            loc,
            cond: Box::new(cond),
            ifarm: Box::new(ifbody),
            elsearm: Box::new(elsebody),
        }))
    }

    /// Reduce the elif chain
    /// <elif-chain> ::= ELIF ( <expr> ) <expr> <elif-chain> | ELSE <expr>
    fn reduce_expr_elif_chain(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();
        match self.remaining() {
            // An elif arm
            [Tk::Elif, ..] => {
                self.take();
                // Parenthesis
                match self.remaining() {
                    [Tk::LPar, ..] => {
                        self.take();
                    }
                    _ => return Err(self.unexpected(&[Tk::LPar])),
                }
                // Cond
                let cond = self.reduce_expr()?;
                // Right parenthesis
                match self.remaining() {
                    [Tk::RPar, ..] => {
                        self.take();
                    }
                    _ => return Err(self.unexpected(&[Tk::RPar])),
                }
                // Body
                let ifbody = self.reduce_expr()?;
                // Else body
                let elsebody = self.reduce_expr_elif_chain()?;

                // Include all the conditional in the error message
                let loc = Loc {
                    start: loc_start,
                    end: self.prev_addr_end().clone(),
                    content: self.text.clone(),
                };
                Ok(Expr::IfExpr(IfExpr {
                    loc,
                    cond: Box::new(cond),
                    ifarm: Box::new(ifbody),
                    elsearm: Box::new(elsebody),
                }))
            }

            // An else arm
            [Tk::Else, ..] => {
                self.take();
                self.reduce_expr()
            }
            _ => Err(self.unexpected(&[Tk::Elif, Tk::Else])),
        }
    }

    /// Reduce a variable(s) declaration
    /// <var-decl> ::= LET [ <var-bind> , ... ] IN <expr>
    fn reduce_expr_var_decl(&mut self) -> PResult<Expr> {
        // Let
        match self.remaining() {
            [Tk::Let, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::Let])),
        }

        // Get the bindings
        self.reduce_expr_var_decl_bind_chain()
    }

    /// Reduce the actual elements in the chain
    fn reduce_expr_var_decl_bind_chain(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();

        let name = match self.remaining() {
            [Tk::Id, ..] => self.take().to_string(),
            _ => return Err(self.unexpected(&[Tk::Id])),
        };

        // Maybe type
        let typ = match self.remaining() {
            [Tk::Colon, ..] => {
                self.take();

                match self.remaining() {
                    [Tk::Id, ..] => Some(self.take().to_string()),
                    _ => return Err(self.unexpected(&[Tk::Id])),
                }
            }
            _ => None,
        };
        // just consider the declaration part, not the expression one
        let loc = Loc {
            start: loc_start,
            end: self.prev_addr_end().clone(),
            content: self.text.clone(),
        };

        // Expect =
        match self.remaining() {
            [Tk::Assign, ..] => {
                self.take();
            }
            _ => return Err(self.unexpected(&[Tk::Assign])),
        }

        // Expect an expression
        let expr = self.reduce_expr()?;

        // What to do next
        let chld = match self.remaining() {
            [Tk::In, ..] => {
                // Another expression
                self.take();
                self.reduce_expr()?
            }
            [Tk::Comma, ..] => {
                // Another binding
                self.take();
                self.reduce_expr_var_decl_bind_chain()?
            }
            _ => return Err(self.unexpected(&[Tk::In, Tk::Comma])),
        };

        // Ok
        Ok(Expr::VarDeclExpr(VarDeclExpr {
            loc,
            name,
            declty: typ,
            expr: Box::new(expr),
            scope: Box::new(chld),
        }))
    }

    /// Reduce a function call
    /// <fn-call> ::=
    fn reduce_expr_fn_call(&mut self) -> PResult<Expr> {
        let loc_start = self.addr_start().clone();

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
            self.take(); // )

            let loc = Loc {
                content: self.text.clone(),
                start: loc_start,
                end: self.prev_addr_end().clone(),
            };
            return Ok(Expr::FunCallExpr(FunCallExpr {
                loc,
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

        let loc = Loc {
            content: self.text.clone(),
            start: loc_start,
            end: self.prev_addr_end().clone(),
        };

        Ok(Expr::FunCallExpr(FunCallExpr {
            loc,
            name: id.into(),
            args,
        }))
    }
}
