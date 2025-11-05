use crate::expression::Expression;
use crate::span::Spanned;

/// FORTRAN statement.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// Assignment statement (variable = expression)
    Assignment {
        variable: Spanned<Expression>,
        value: Spanned<Expression>,
    },
    
    /// IF statement
    If {
        condition: Spanned<Expression>,
        then_statements: Vec<Spanned<Statement>>,
        else_statements: Option<Vec<Spanned<Statement>>>,
        else_if_clauses: Vec<ElseIfClause>,
    },
    
    /// DO loop
    Do {
        variable: Option<String>,
        start: Option<Spanned<Expression>>,
        end: Option<Spanned<Expression>>,
        step: Option<Spanned<Expression>>,
        statements: Vec<Spanned<Statement>>,
    },
    
    /// DO WHILE loop
    DoWhile {
        condition: Spanned<Expression>,
        statements: Vec<Spanned<Statement>>,
    },
    
    /// SELECT CASE statement
    SelectCase {
        expression: Spanned<Expression>,
        cases: Vec<CaseClause>,
        default: Option<Vec<Spanned<Statement>>>,
    },
    
    /// CALL statement
    Call {
        subroutine_name: String,
        arguments: Vec<Spanned<Expression>>,
    },
    
    /// RETURN statement
    Return {
        expression: Option<Spanned<Expression>>,
    },
    
    /// STOP statement
    Stop {
        code: Option<Spanned<Expression>>,
    },
    
    /// CONTINUE statement
    Continue,
    
    /// CYCLE statement (continue to next iteration)
    Cycle,
    
    /// EXIT statement (exit loop)
    Exit,
    
    /// READ statement
    Read {
        unit: Option<Spanned<Expression>>,
        format: Option<Spanned<Expression>>,
        iostat: Option<String>,
        iomsg: Option<String>,
        variables: Vec<Spanned<Expression>>,
    },
    
    /// WRITE statement
    Write {
        unit: Option<Spanned<Expression>>,
        format: Option<Spanned<Expression>>,
        iostat: Option<String>,
        iomsg: Option<String>,
        expressions: Vec<Spanned<Expression>>,
    },
    
    /// PRINT statement
    Print {
        format: Option<Spanned<Expression>>,
        expressions: Vec<Spanned<Expression>>,
    },
    
    /// OPEN statement
    Open {
        unit: Spanned<Expression>,
        file: Option<Spanned<Expression>>,
        status: Option<String>,
        action: Option<String>,
        form: Option<String>,
        access: Option<String>,
    },
    
    /// CLOSE statement
    Close {
        unit: Spanned<Expression>,
        status: Option<String>,
        iostat: Option<String>,
    },
    
    /// ALLOCATE statement
    Allocate {
        variables: Vec<AllocateSpec>,
        stat: Option<String>,
        errmsg: Option<String>,
    },
    
    /// DEALLOCATE statement
    Deallocate {
        variables: Vec<Spanned<Expression>>,
        stat: Option<String>,
        errmsg: Option<String>,
    },
    
    /// NULLIFY statement
    Nullify {
        pointers: Vec<Spanned<Expression>>,
    },
}

/// ELSE IF clause in IF statement.
#[derive(Debug, Clone, PartialEq)]
pub struct ElseIfClause {
    pub condition: Spanned<Expression>,
    pub statements: Vec<Spanned<Statement>>,
}

/// CASE clause in SELECT CASE statement.
#[derive(Debug, Clone, PartialEq)]
pub struct CaseClause {
    pub selector: CaseSelector,
    pub statements: Vec<Spanned<Statement>>,
}

/// CASE selector (value, range, or DEFAULT).
#[derive(Debug, Clone, PartialEq)]
pub enum CaseSelector {
    Value(Spanned<Expression>),
    Range {
        start: Spanned<Expression>,
        end: Spanned<Expression>,
    },
    Default,
}

/// ALLOCATE specification.
#[derive(Debug, Clone, PartialEq)]
pub struct AllocateSpec {
    pub variable: Spanned<Expression>,
    pub shape: Option<Vec<Spanned<Expression>>>,
}

