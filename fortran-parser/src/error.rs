use fortran_lexer::LexError;
use fortran_lexer::Token;

/// Parser error.
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    /// Unexpected token found.
    UnexpectedToken {
        expected: Vec<String>,
        found: Token,
    },
    /// Unexpected end of file.
    UnexpectedEof {
        expected: Vec<String>,
    },
    /// Lexer error.
    LexerError(LexError),
    /// Invalid syntax.
    InvalidSyntax {
        message: String,
        line: usize,
        column: usize,
    },
}

pub type ParseResult<T> = Result<T, ParseError>;

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken { expected, found } => {
                write!(
                    f,
                    "Unexpected token {:?} at line {}, column {}. Expected: {}",
                    found.token_type,
                    found.line,
                    found.column,
                    expected.join(" or ")
                )
            }
            ParseError::UnexpectedEof { expected } => {
                write!(
                    f,
                    "Unexpected end of file. Expected: {}",
                    expected.join(" or ")
                )
            }
            ParseError::LexerError(err) => {
                write!(f, "Lexer error: {}", err)
            }
            ParseError::InvalidSyntax { message, line, column } => {
                write!(f, "Invalid syntax at line {}, column {}: {}", line, column, message)
            }
        }
    }
}

impl std::error::Error for ParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ParseError::LexerError(err) => Some(err),
            _ => None,
        }
    }
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        ParseError::LexerError(err)
    }
}

