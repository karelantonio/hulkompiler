//! Lexer, parser and IR implementations for the HULK (https://github.com/matcom/hulk) programming
//! language(s)

/// The lexer module
pub mod lex;

/// The parser module
pub mod ast;

/// The high-level intermediate representation
pub mod hir;

/// To emit code to other languages (transpile)
pub mod emit;
