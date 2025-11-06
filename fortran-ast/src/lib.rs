//! Abstract Syntax Tree (AST) data structures for FORTRAN programs.
//!
//! This crate provides comprehensive data structures for representing FORTRAN programs
//! as Abstract Syntax Trees, with source location tracking and visitor pattern support.
//!
//! # Features
//!
//! - ✅ Complete AST representation of FORTRAN program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
//! - ✅ Declaration structures (variable declarations, type specifications, attributes)
//! - ✅ Expression trees (arithmetic, logical, comparison, function calls)
//! - ✅ Statement structures (IF, DO, SELECT CASE, I/O statements, etc.)
//! - ✅ Source span tracking for all nodes (`Spanned<T>`)
//! - ✅ Visitor pattern for AST traversal
//! - ✅ Optional serialization support (serde feature)
//!
//! # AST Structure
//!
//! ```rust
//! use fortran_ast::{Program, ProgramUnit, MainProgram, Statement, Expression, Literal};
//!
//! // Create a simple AST node
//! let expr = Expression::Literal(Literal::integer("42"));
//! println!("Expression: {:?}", expr);
//!
//! // Working with spanned nodes (includes source location)
//! use fortran_ast::{Span, Spanned};
//! let span = Span::new(0, 9, 1, 10); // bytes 0-9, line 1, col 10
//! let spanned_expr = Spanned::new(expr, span);
//! println!("Expression at {:?}", spanned_expr.span);
//! ```
//!
//! # Visitor Pattern
//!
//! ```rust
//! use fortran_ast::{Visitor, Expression, Statement, Declaration};
//!
//! struct ExpressionCounter {
//!     count: usize,
//! }
//!
//! impl Visitor for ExpressionCounter {
//!     fn visit_expression(&mut self, _expr: &Expression) {
//!         self.count += 1;
//!     }
//! }
//!
//! let mut counter = ExpressionCounter { count: 0 };
//! // Visit AST nodes...
//! println!("Found {} expressions", counter.count);
//! ```
//!
//! # Serialization
//!
//! When the `serialize` feature is enabled, all AST nodes can be serialized:
//!
//! ```rust,ignore
//! use fortran_ast::Expression;
//! use serde_json;
//!
//! let expr = Expression::Literal(fortran_ast::Literal::integer("42"));
//! let json = serde_json::to_string(&expr)?;
//! println!("Serialized: {}", json);
//! ```

pub mod analysis;
pub mod declaration;
pub mod expression;
pub mod literal;
pub mod program;
pub mod span;
pub mod statement;
pub mod visitor;

pub use analysis::{SymbolTable, CallGraph, Symbol, SymbolType, ProcedureInfo};
pub use declaration::{Declaration, TypeSpec, Attribute, Intent};
pub use expression::{Expression, UnaryOp, BinaryOp};
pub use literal::Literal;
pub use program::{Program, ProgramUnit, MainProgram, Subroutine, Function, Module};
pub use span::{Span, Spanned};
pub use statement::Statement;
pub use visitor::{Visitor, AnalysisVisitor};
