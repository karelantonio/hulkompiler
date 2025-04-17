//! Split the input data into tokens
use logos::Logos;
use thiserror::Error;

/// Error lexing
#[derive(Error, Debug)]
pub enum LexError {
    #[error("Unexpected char at line: {line}, column: {col} (near ...{hint}...)")]
    Unexpected {
        line: usize,
        col: usize,
        hint: String,
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

#[derive(Debug, Clone, Copy)]
pub struct Addr {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct LocTk<'a> {
    pub start: Addr,
    pub end: Addr,
    pub slice: &'a str,
    pub tk: Tk,
}

/// Tokenize the data and report any error found, return a list of: (line, token, slice)
pub fn tokenize_data<'a>(data: &'a str) -> Result<Vec<LocTk<'a>>, LexError> {
    let mut tk = Tk::lexer(data);
    let mut res = Vec::new();
    let (mut line, mut prevlen) = (1usize, 0usize);

    while let Some(tok) = tk.next() {
        let Ok(tok) = tok else {
            return Err(LexError::Unexpected {
                line,
                col: tk.span().start - prevlen + 1,
                hint: tk.slice().into(),
            });
        };

        if let Tk::Nl = tok {
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
