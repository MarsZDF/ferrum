use crate::literal::Literal;
use crate::span::Spanned;

/// FORTRAN expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    /// Literal value
    Literal(Literal),
    
    /// Variable reference
    Variable(String),
    
    /// Array element reference (variable(subscripts))
    ArrayElement {
        variable: String,
        subscripts: Vec<Spanned<Expression>>,
    },
    
    /// Function call
    FunctionCall {
        name: String,
        arguments: Vec<Spanned<Expression>>,
    },
    
    /// Unary expression (-x, +x, .NOT. x)
    Unary {
        operator: UnaryOp,
        operand: Box<Spanned<Expression>>,
    },
    
    /// Binary expression (x + y, x .AND. y, etc.)
    Binary {
        operator: BinaryOp,
        left: Box<Spanned<Expression>>,
        right: Box<Spanned<Expression>>,
    },
    
    /// Array constructor ([expr1, expr2, ...])
    ArrayConstructor(Vec<Spanned<Expression>>),
}

/// Unary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,   // +
    Minus,  // -
    Not,    // .NOT.
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Power,    // **
    
    // Comparison
    Equal,          // ==
    NotEqual,       // /=
    LessThan,       // <
    LessOrEqual,    // <=
    GreaterThan,    // >
    GreaterOrEqual, // >=
    
    // Logical
    And,  // .AND.
    Or,   // .OR.
    Eqv,  // .EQV.
    Neqv, // .NEQV.
    
    // String concatenation
    Concat, // //
}

