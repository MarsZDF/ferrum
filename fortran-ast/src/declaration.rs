use crate::expression::Expression;
use crate::span::Spanned;

/// FORTRAN declaration.
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    /// Variable declaration
    Variable {
        name: String,
        type_spec: TypeSpec,
        attributes: Vec<Attribute>,
        initializer: Option<Spanned<Expression>>,
    },
    
    /// PARAMETER declaration
    Parameter {
        name: String,
        type_spec: Option<TypeSpec>,
        value: Spanned<Expression>,
    },
    
    /// IMPLICIT statement
    Implicit {
        rules: Vec<ImplicitRule>,
    },
    
    /// DIMENSION statement
    Dimension {
        declarations: Vec<DimensionDecl>,
    },
    
    /// COMMON block declaration
    Common {
        block_name: Option<String>,
        variables: Vec<String>,
    },
    
    /// EQUIVALENCE statement
    Equivalence {
        groups: Vec<Vec<String>>,
    },
    
    /// EXTERNAL statement
    External {
        names: Vec<String>,
    },
    
    /// INTRINSIC statement
    Intrinsic {
        names: Vec<String>,
    },
    
    /// SAVE statement
    Save {
        variables: Option<Vec<String>>,
    },
}

/// Type specification.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeSpec {
    Integer {
        kind: Option<Spanned<Expression>>,
    },
    Real {
        kind: Option<Spanned<Expression>>,
    },
    DoublePrecision,
    Complex {
        kind: Option<Spanned<Expression>>,
    },
    Character {
        length: Option<Spanned<Expression>>,
        kind: Option<Spanned<Expression>>,
    },
    Logical {
        kind: Option<Spanned<Expression>>,
    },
}

/// Variable attribute.
#[derive(Debug, Clone, PartialEq)]
pub enum Attribute {
    Allocatable,
    Dimension(Vec<Spanned<Expression>>),
    Intent(Intent),
    Optional,
    Pointer,
    Target,
    Save,
    Parameter,
    Public,
    Private,
}

/// INTENT attribute.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Intent {
    In,
    Out,
    InOut,
}

/// IMPLICIT rule.
#[derive(Debug, Clone, PartialEq)]
pub struct ImplicitRule {
    pub type_spec: TypeSpec,
    pub letters: LetterRange,
}

/// Letter range (e.g., 'A-Z').
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetterRange {
    pub start: char,
    pub end: char,
}

/// DIMENSION declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct DimensionDecl {
    pub name: String,
    pub dimensions: Vec<Spanned<Expression>>,
}

