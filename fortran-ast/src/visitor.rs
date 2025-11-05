use crate::declaration::Declaration;
use crate::expression::Expression;
use crate::program::Program;
use crate::statement::Statement;

/// Visitor trait for traversing the AST.
pub trait Visitor {
    fn visit_program(&mut self, program: &Program) {
        self.visit_program_unit(&program.program_unit);
    }
    
    fn visit_program_unit(&mut self, unit: &crate::program::ProgramUnit) {
        match unit {
            crate::program::ProgramUnit::MainProgram(mp) => self.visit_main_program(mp),
            crate::program::ProgramUnit::Subroutine(s) => self.visit_subroutine(s),
            crate::program::ProgramUnit::Function(f) => self.visit_function(f),
            crate::program::ProgramUnit::Module(m) => self.visit_module(m),
        }
    }
    
    fn visit_main_program(&mut self, _program: &crate::program::MainProgram) {}
    fn visit_subroutine(&mut self, _subroutine: &crate::program::Subroutine) {}
    fn visit_function(&mut self, _function: &crate::program::Function) {}
    fn visit_module(&mut self, _module: &crate::program::Module) {}
    
    fn visit_declaration(&mut self, _declaration: &Declaration) {}
    
    fn visit_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Assignment { .. } => {}
            Statement::If { .. } => {}
            Statement::Do { .. } => {}
            Statement::DoWhile { .. } => {}
            Statement::SelectCase { .. } => {}
            Statement::Call { .. } => {}
            Statement::Return { .. } => {}
            Statement::Stop { .. } => {}
            Statement::Continue => {}
            Statement::Cycle => {}
            Statement::Exit => {}
            Statement::Read { .. } => {}
            Statement::Write { .. } => {}
            Statement::Print { .. } => {}
            Statement::Open { .. } => {}
            Statement::Close { .. } => {}
            Statement::Allocate { .. } => {}
            Statement::Deallocate { .. } => {}
            Statement::Nullify { .. } => {}
        }
    }
    
    fn visit_expression(&mut self, _expression: &Expression) {}
}

