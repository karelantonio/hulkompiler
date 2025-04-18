//! Split the input data into tokens
use logos::Logos;
use thiserror::Error;
use hulkompiler_sourcehint::{LocError, make};

/// Error lexing, usually an unexpected token. It uses [`hulkompiler_sourcehint`] to make pretty
/// error messages.
#[derive(Error, Debug)]
pub enum LexError {
    #[error("Unexpected char at line: {line}, column: {col}")]
    Unexpected {
        line: usize,
        col: usize,
        #[source]
        loc: LocError,
    },
}

/// The tokens
#[derive(Debug, Clone, Copy, PartialEq, Logos)]
#[logos(skip r"[ \t\r\f]+")]
pub enum Tk {
    // New line (ignored later)
    #[token("\n")]
    Nl,

    // Keywords
    #[token("function")]
    Function,

    #[token("let")]
    Let,

    #[token("in")]
    In,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("if")]
    If,

    #[token("elif")]
    Elif,

    #[token("else")]
    Else,

    #[token("while")]
    While,

    // Operators
    #[token("+")]
    Add,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("^")]
    Power,

    #[token("@")]
    Cat,

    // Comparison symbols
    #[token("==")]
    Eq,

    #[token("!=")]
    Neq,

    #[token("<=")]
    Le,

    #[token(">=")]
    Ge,

    #[token("<")]
    Lt,

    #[token(">")]
    Gt,

    // Logic symbols
    #[token("&")]
    Amp,

    #[token("|")]
    Pipe,

    #[token("!")]
    Excl,

    // Other symbols
    #[token("=>")]
    RArrow,

    #[token(":=")]
    Reassign,

    #[token("=")]
    Assign,

    #[token("(")]
    LPar,

    #[token(")")]
    RPar,

    #[token("{")]
    LBrac,

    #[token("}")]
    RBrac,

    #[token(",")]
    Comma,

    #[token(":")]
    Colon,

    #[token(";")]
    Semicolon,

    #[regex(r#"\"(\\"|[^"\n])*\""#)]
    Str,

    // Identifiers
    #[regex(r"[a-zA-Z][a-zA-Z0-9_]*")]
    Id,

    #[regex(r"(0|[1-9][0-9]*)(\.[0-9]+)?")]
    Num,
}

impl core::fmt::Display for Tk {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", match self {
            Tk::Nl => "new line",
            Tk::Function => "function",
            Tk::Let => "let",
            Tk::In => "in",
            Tk::True => "true",
            Tk::False => "false",
            Tk::If => "if",
            Tk::Elif => "elif",
            Tk::Else => "else",
            Tk::While => "while",
            Tk::Add => "`+`",
            Tk::Minus => "`-`",
            Tk::Star => "`*`",
            Tk::Slash => "`/`",
            Tk::Power => "`^`",
            Tk::Cat => "`@`",
            Tk::Eq => "`==`",
            Tk::Neq => "`!=`",
            Tk::Le => "`<=`",
            Tk::Ge => "`>=`",
            Tk::Lt => "`<`",
            Tk::Gt => "`>`",
            Tk::Amp => "`&`",
            Tk::Pipe => "`|`",
            Tk::Excl => "`!`",
            Tk::RArrow => "`=>`",
            Tk::Reassign => "`:=`",
            Tk::Assign => "`=`",
            Tk::LPar => "`(`",
            Tk::RPar => "`)`",
            Tk::LBrac => "`{`",
            Tk::RBrac => "`}`",
            Tk::Comma => "`,`",
            Tk::Colon => "`:`",
            Tk::Semicolon => "`;`",
            Tk::Str => "string",
            Tk::Id => "identifier",
            Tk::Num => "number",
        })
    }
}

/// Address in the source file, lines and columns should start at 1, not 0.
#[derive(Debug, Clone, Copy)]
pub struct Addr {
    pub line: usize,
    pub col: usize,
}

/// A token with its address in the source associated.
#[derive(Debug, Clone, Copy)]
pub struct LocTk<'a> {
    pub start: Addr,
    pub end: Addr,
    pub slice: &'a str,
    pub tk: Tk,
}


fn make_loc_err(lines: Vec<String>, line: usize, sc: usize, ec: usize) -> LocError {
    make(&lines, line, sc, line, ec)
}


/// Tokenize the data and report any error found, return a list of [`LocTk`]
pub fn tokenize_data<'a>(data: &'a str) -> Result<Vec<LocTk<'a>>, LexError> {

    let mut tk = Tk::lexer(data);
    let mut res = Vec::new();
    let (mut line, mut prevlen) = (1usize, 0usize);

    while let Some(tok) = tk.next() {
        let Ok(tok) = tok else {
            // Clone the data, for the LocError
            let lines = data.split("\n").map(|e| e.to_string()).collect();

            // Start and end column (starting from 1)
            let startcol = tk.span().start - prevlen;
            let endcol = tk.span().end - 1 - prevlen;

            return Err(LexError::Unexpected {
                line,
                col: tk.span().start - prevlen + 1,
                loc: make_loc_err(lines, line, startcol, endcol),
            });
        };

        if let Tk::Nl = tok {
            // Dont include new lines, just increment the line counter
            line += 1;
            prevlen = tk.span().end - 1;
            continue;
        }

        res.push(LocTk {
            start: Addr {
                line,
                col: tk.span().start - prevlen,
            },
            end: Addr {
                line,
                col: tk.span().end - 1 - prevlen,
            },
            slice: tk.slice(),
            tk: tok,
        });
    }

    Ok(res)
}
