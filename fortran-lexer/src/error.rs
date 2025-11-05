use std::fmt;

/// Lexical analysis error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    /// Unexpected character at a specific location.
    UnexpectedChar {
        ch: char,
        line: usize,
        column: usize,
    },
    /// Unterminated string literal.
    UnterminatedString {
        line: usize,
        column: usize,
    },
    /// Invalid numeric literal.
    InvalidNumericLiteral {
        value: String,
        line: usize,
        column: usize,
    },
    /// Invalid character in identifier.
    InvalidIdentifier {
        value: String,
        line: usize,
        column: usize,
    },
}

pub type LexResult<T> = Result<T, LexError>;

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::UnexpectedChar { ch, line, column } => {
                write!(f, "Unexpected character '{}' at line {}, column {}", ch, line, column)
            }
            LexError::UnterminatedString { line, column } => {
                write!(f, "Unterminated string literal at line {}, column {}", line, column)
            }
            LexError::InvalidNumericLiteral { value, line, column } => {
                write!(
                    f,
                    "Invalid numeric literal '{}' at line {}, column {}",
                    value, line, column
                )
            }
            LexError::InvalidIdentifier { value, line, column } => {
                write!(
                    f,
                    "Invalid identifier '{}' at line {}, column {}",
                    value, line, column
                )
            }
        }
    }
}

impl std::error::Error for LexError {}

