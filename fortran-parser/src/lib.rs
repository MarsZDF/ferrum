//! Recursive descent parser for FORTRAN source code.
//!
//! This crate provides a comprehensive parser that converts FORTRAN source code into 
//! an Abstract Syntax Tree (AST) using recursive descent parsing with precedence climbing.
//!
//! # Features
//!
//! - ✅ Parses FORTRAN program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
//! - ✅ Parses declarations (variable declarations, type specifications, attributes)
//! - ✅ Parses executable statements (IF, DO, READ, WRITE, PRINT, RETURN, STOP, etc.)
//! - ✅ Parses expressions (arithmetic, logical, comparison, function calls)
//! - ✅ Error reporting with location information
//! - ✅ Handles whitespace and comments gracefully
//!
//! # Quick Start
//!
//! ```rust
//! use fortran_parser::parse;
//!
//! let source = r#"
//! program factorial
//!     implicit none
//!     integer :: n, result
//!     n = 5
//!     result = 1
//!     do i = 1, n
//!         result = result * i
//!     end do
//!     print *, 'Factorial of', n, 'is', result
//! end program factorial
//! "#;
//!
//! let program = parse(source)?;
//! println!("Parsed program: {:?}", program);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! # Error Handling
//!
//! ```rust
//! use fortran_parser::{parse, ParseError};
//!
//! let invalid_source = "program test\n    if x > 0  // missing 'then'";
//! match parse(invalid_source) {
//!     Ok(program) => println!("Parsed successfully"),
//!     Err(ParseError::UnexpectedToken { expected, found }) => {
//!         println!("Parse error: expected {:?}, found {:?}", expected, found);
//!     }
//!     Err(err) => println!("Parse error: {:?}", err),
//! }
//! ```
//!
//! # Working with the AST
//!
//! ```rust
//! use fortran_parser::parse;
//! use fortran_ast::{ProgramUnit, Statement};
//!
//! let source = r#"
//! program simple
//!     x = 42
//!     y = x + 1
//! end program simple
//! "#;
//!
//! let program = parse(source)?;
//! if let ProgramUnit::MainProgram(main_prog) = program.program_unit {
//!     println!("Program name: {:?}", main_prog.name);
//!     println!("Number of statements: {}", main_prog.executable_statements.len());
//!     
//!     for statement in &main_prog.executable_statements {
//!         match &statement.node {
//!             Statement::Assignment { variable, value } => {
//!                 println!("Assignment: {:?} = {:?}", variable, value);
//!             }
//!             _ => println!("Other statement: {:?}", statement.node),
//!         }
//!     }
//! }
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```

pub mod error;
pub mod parser;

pub use error::{ParseError, ParseResult};
pub use parser::{parse, Parser};
