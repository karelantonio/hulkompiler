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
#[derive(Debug, PartialEq, Logos)]
#[logos(skip r"[ \t\r\f]+")]
pub enum Tk {
    // New line (ignored later)
    #[token("\n")]
    Nl,

    // Operators
    #[token("+")]
    Add,

    #[token("-")]
    Minus,

    #[token("*")]
    Star,

    #[token("/")]
    Slash,

    #[token("%")]
    Perc,

    // Other symbols
    #[token("(")]
    LPar,

    #[token(")")]
    RPar,

    // Identifiers
    #[regex(r"[a-zA-Z][a-zA-Z0-9_]*")]
    Id,

    #[regex(r"0|[1-9][0-9]*")]
    Num,
}

/// Tokenize the data and report any error found, return a list of: (line, token, slice)
pub fn tokenize_data<'a>(data: &'a str) -> Result<Vec<(usize, Tk, &'a str)>, LexError> {
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

        if let Tk::Nl = tok{
            line += 1;
            prevlen = tk.span().end;
            continue;
        }

        res.push((line, tok, tk.slice()));
    }

    Ok(res)
}
