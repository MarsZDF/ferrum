//! Fortran lexer library for tokenizing FORTRAN source code.
//!
//! Supports both fixed-format and free-format FORTRAN.

pub mod error;
pub mod lexer;
pub mod token;

pub use error::{LexError, LexResult};
pub use lexer::{tokenize, detect_format, Format};
pub use token::{Token, TokenType};
