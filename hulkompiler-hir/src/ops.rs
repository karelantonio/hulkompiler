//! Unary and binary operators, see [`Op`], [`UOp`]

/// A binary operation
#[derive(Debug, Eq, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Cat,
    Le,
    Ge,
    Lt,
    Gt,
    Eq,
    Neq,
    And,
    Or,
}

impl Op {
    
    /// Check if a given operator is arithmetic
    pub fn is_arithmetic(&self) -> bool {
        match self {
            Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Pow
            | Self::Le
            | Self::Ge
            | Self::Lt
            | Self::Gt => true,
            _ => false,
        }
    }

    /// Is a comparison operator
    pub fn is_comparison(&self) -> bool {
        match self {
            Self::Le | Self::Lt | Self::Ge | Self::Gt | Self::Eq | Self::Neq => true,
            _ => false,
        }
    }

    /// Is a logic operator
    pub fn is_logic(&self) -> bool {
        match self {
            Self::And | Self::Or => true,
            _ => false,
        }
    }
}

/// Unary operator
#[derive(Debug)]
pub enum UOp {
    Neg, // additive inverse
    Not, // logical negation
}
