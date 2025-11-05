//! FORTRAN Abstract Syntax Tree (AST) library.
//!
//! Provides data structures for representing FORTRAN programs as ASTs.

pub mod declaration;
pub mod expression;
pub mod literal;
pub mod program;
pub mod span;
pub mod statement;
pub mod visitor;

pub use declaration::{Declaration, TypeSpec, Attribute, Intent};
pub use expression::{Expression, UnaryOp, BinaryOp};
pub use literal::Literal;
pub use program::{Program, ProgramUnit, MainProgram, Subroutine, Function, Module};
pub use span::{Span, Spanned};
pub use statement::Statement;
pub use visitor::Visitor;
