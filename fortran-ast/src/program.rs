use crate::declaration::Declaration;
use crate::statement::Statement;
use crate::span::Spanned;

/// Complete FORTRAN program.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub name: Option<String>,
    pub program_unit: ProgramUnit,
}

/// FORTRAN program unit.
#[derive(Debug, Clone, PartialEq)]
pub enum ProgramUnit {
    MainProgram(MainProgram),
    Subroutine(Subroutine),
    Function(Function),
    Module(Module),
}

/// Main program.
#[derive(Debug, Clone, PartialEq)]
pub struct MainProgram {
    pub name: Option<String>,
    pub declarations: Vec<Spanned<Declaration>>,
    pub executable_statements: Vec<Spanned<Statement>>,
    pub contains: Option<ContainsSection>,
}

/// Subroutine.
#[derive(Debug, Clone, PartialEq)]
pub struct Subroutine {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub declarations: Vec<Spanned<Declaration>>,
    pub executable_statements: Vec<Spanned<Statement>>,
    pub contains: Option<ContainsSection>,
}

/// Function.
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub result_name: Option<String>,
    pub type_spec: Option<crate::declaration::TypeSpec>,
    pub declarations: Vec<Spanned<Declaration>>,
    pub executable_statements: Vec<Spanned<Statement>>,
    pub contains: Option<ContainsSection>,
}

/// Module.
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub declarations: Vec<Spanned<Declaration>>,
    pub contains: Option<ContainsSection>,
}

/// CONTAINS section.
#[derive(Debug, Clone, PartialEq)]
pub struct ContainsSection {
    pub internal_procedures: Vec<InternalProcedure>,
}

/// Internal procedure (subroutine or function within a program unit).
#[derive(Debug, Clone, PartialEq)]
pub enum InternalProcedure {
    Subroutine(Subroutine),
    Function(Function),
}

/// Procedure argument.
#[derive(Debug, Clone, PartialEq)]
pub struct Argument {
    pub name: String,
    pub intent: Option<crate::declaration::Intent>,
    pub optional: bool,
}

