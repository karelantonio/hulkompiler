//! Contains some types and utilities

/// A type (may be inside a type pool)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ty {
    Obj,
    Num,
    Str,
    Bool,
}

