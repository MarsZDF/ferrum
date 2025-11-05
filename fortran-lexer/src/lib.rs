//! Fast, modular lexer for FORTRAN source code supporting both fixed-format and free-format FORTRAN.
//!
//! This crate provides comprehensive tokenization of FORTRAN source code with precise source
//! location tracking and comprehensive error reporting.
//!
//! # Features
//!
//! - ✅ Free-format FORTRAN lexing (FORTRAN 90+)
//! - ✅ Case-insensitive keyword recognition
//! - ✅ Comprehensive token types (keywords, identifiers, literals, operators, punctuation)
//! - ✅ Source location tracking (line, column, span)
//! - ✅ Error reporting with precise location information
//! - ✅ Format detection (fixed vs free format)
//! - 🚧 Fixed-format FORTRAN lexing (in progress)
//!
//! # Quick Start
//!
//! ```rust
//! use fortran_lexer::{tokenize, Format};
//!
//! let source = r#"
//! program hello_world
//!     implicit none
//!     print *, 'Hello, World!'
//! end program hello_world
//! "#;
//!
//! let tokens = tokenize(source, Format::FreeFormat)?;
//! for token in tokens {
//!     if !token.is_trivial() {
//!         println!("{:?} at line {}:{}", token.token_type, token.line, token.column);
//!     }
//! }
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! # Error Handling
//!
//! ```rust
//! use fortran_lexer::{tokenize, Format, LexError};
//!
//! let invalid_source = "program test\n    x = 'unterminated string";
//! match tokenize(invalid_source, Format::FreeFormat) {
//!     Ok(tokens) => println!("Tokenized successfully: {} tokens", tokens.len()),
//!     Err(LexError::UnterminatedString { line, column }) => {
//!         println!("Unterminated string at line {}, column {}", line, column);
//!     }
//!     Err(err) => println!("Lexing error: {:?}", err),
//! }
//! ```

pub mod error;
pub mod lexer;
pub mod token;

pub use error::{LexError, LexResult};
pub use lexer::{tokenize, detect_format, Format};
pub use token::{Token, TokenType};
