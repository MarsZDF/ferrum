//! FORTRAN parser library.
//!
//! Provides a recursive descent parser for FORTRAN source code that converts
//! tokens into an Abstract Syntax Tree (AST).

pub mod error;
pub mod parser;

pub use error::{ParseError, ParseResult};
pub use parser::{parse, Parser};
