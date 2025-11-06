use crate::declaration::Declaration;
use crate::expression::Expression;
use crate::program::Program;
use crate::statement::Statement;
use crate::analysis::{SymbolTable, CallGraph};

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

/// Analysis visitor trait for building symbol tables and call graphs
pub trait AnalysisVisitor {
    /// Get mutable access to symbol table
    fn symbol_table_mut(&mut self) -> &mut SymbolTable;
    
    /// Get mutable access to call graph
    fn call_graph_mut(&mut self) -> &mut CallGraph;
    
    /// Visit program with analysis
    fn analyze_program(&mut self, program: &Program) {
        self.analyze_program_unit(&program.program_unit);
    }
    
    /// Analyze a program unit
    fn analyze_program_unit(&mut self, unit: &crate::program::ProgramUnit) {
        match unit {
            crate::program::ProgramUnit::MainProgram(mp) => self.analyze_main_program(mp),
            crate::program::ProgramUnit::Subroutine(s) => self.analyze_subroutine(s),
            crate::program::ProgramUnit::Function(f) => self.analyze_function(f),
            crate::program::ProgramUnit::Module(m) => self.analyze_module(m),
        }
    }
    
    /// Analyze main program
    fn analyze_main_program(&mut self, program: &crate::program::MainProgram) {
        use crate::analysis::{ScopeType, Symbol, SymbolType, SymbolMetadata, ProcedureInfo};
        
        // Enter program scope
        let scope_name = program.name.as_deref().unwrap_or("main");
        self.symbol_table_mut().enter_scope(ScopeType::Program);
        
        // Add to call graph as main entry point
        let proc_info = ProcedureInfo {
            name: scope_name.to_string(),
            defined_at: crate::span::Span::new(0, 0, 1, 1), // TODO: proper span
            parameters: Vec::new(),
            is_main: true,
            is_external: false,
        };
        self.call_graph_mut().add_procedure(proc_info);
        
        // Analyze declarations
        for decl in &program.declarations {
            self.analyze_declaration(&decl.node);
        }
        
        // Analyze statements
        for stmt in &program.executable_statements {
            self.analyze_statement(&stmt.node);
        }
        
        // Analyze internal procedures if present
        if let Some(contains) = &program.contains {
            self.analyze_contains_section(contains);
        }
        
        // Exit scope
        self.symbol_table_mut().exit_scope();
    }
    
    /// Analyze subroutine
    fn analyze_subroutine(&mut self, subroutine: &crate::program::Subroutine) {
        use crate::analysis::{ScopeType, Symbol, SymbolType, SymbolMetadata, ProcedureInfo};
        
        // Enter subroutine scope
        self.symbol_table_mut().enter_scope(ScopeType::Subroutine(subroutine.name.clone()));
        
        // Add to call graph
        let parameters: Vec<String> = subroutine.arguments.iter()
            .map(|arg| arg.name.clone())
            .collect();
            
        let proc_info = ProcedureInfo {
            name: subroutine.name.clone(),
            defined_at: crate::span::Span::new(0, 0, 1, 1), // TODO: proper span
            parameters: parameters.clone(),
            is_main: false,
            is_external: false,
        };
        self.call_graph_mut().add_procedure(proc_info);
        
        // Define subroutine symbol
        let sub_symbol = Symbol {
            name: subroutine.name.clone(),
            symbol_type: SymbolType::Subroutine { parameters },
            defined_at: crate::span::Span::new(0, 0, 1, 1), // TODO: proper span
            used_at: Vec::new(),
            metadata: SymbolMetadata::default(),
        };
        self.symbol_table_mut().define_symbol(sub_symbol);
        
        // Define parameter symbols
        for arg in &subroutine.arguments {
            let param_symbol = Symbol {
                name: arg.name.clone(),
                symbol_type: SymbolType::Parameter,
                defined_at: crate::span::Span::new(0, 0, 1, 1), // TODO: proper span
                used_at: Vec::new(),
                metadata: SymbolMetadata { is_used: true, ..Default::default() },
            };
            self.symbol_table_mut().define_symbol(param_symbol);
        }
        
        // Analyze declarations
        for decl in &subroutine.declarations {
            self.analyze_declaration(&decl.node);
        }
        
        // Analyze statements
        for stmt in &subroutine.executable_statements {
            self.analyze_statement(&stmt.node);
        }
        
        // Analyze internal procedures if present
        if let Some(contains) = &subroutine.contains {
            self.analyze_contains_section(contains);
        }
        
        // Exit scope
        self.symbol_table_mut().exit_scope();
    }
    
    /// Analyze function  
    fn analyze_function(&mut self, function: &crate::program::Function) {
        use crate::analysis::{ScopeType, Symbol, SymbolType, SymbolMetadata, ProcedureInfo};
        
        // Enter function scope
        self.symbol_table_mut().enter_scope(ScopeType::Function(function.name.clone()));
        
        // Add to call graph
        let parameters: Vec<String> = function.arguments.iter()
            .map(|arg| arg.name.clone())
            .collect();
            
        let proc_info = ProcedureInfo {
            name: function.name.clone(),
            defined_at: crate::span::Span::new(0, 0, 1, 1), // TODO: proper span
            parameters: parameters.clone(),
            is_main: false,
            is_external: false,
        };
        self.call_graph_mut().add_procedure(proc_info);
        
        // Define function symbol
        let func_symbol = Symbol {
            name: function.name.clone(),
            symbol_type: SymbolType::Function { 
                parameters,
                return_type: function.result_name.clone().unwrap_or_else(|| function.name.clone())
            },
            defined_at: crate::span::Span::new(0, 0, 1, 1), // TODO: proper span
            used_at: Vec::new(),
            metadata: SymbolMetadata::default(),
        };
        self.symbol_table_mut().define_symbol(func_symbol);
        
        // Define parameter symbols
        for arg in &function.arguments {
            let param_symbol = Symbol {
                name: arg.name.clone(),
                symbol_type: SymbolType::Parameter,
                defined_at: crate::span::Span::new(0, 0, 1, 1), // TODO: proper span
                used_at: Vec::new(),
                metadata: SymbolMetadata { is_used: true, ..Default::default() },
            };
            self.symbol_table_mut().define_symbol(param_symbol);
        }
        
        // Analyze declarations
        for decl in &function.declarations {
            self.analyze_declaration(&decl.node);
        }
        
        // Analyze statements
        for stmt in &function.executable_statements {
            self.analyze_statement(&stmt.node);
        }
        
        // Analyze internal procedures if present
        if let Some(contains) = &function.contains {
            self.analyze_contains_section(contains);
        }
        
        // Exit scope
        self.symbol_table_mut().exit_scope();
    }
    
    /// Analyze module
    fn analyze_module(&mut self, _module: &crate::program::Module) {
        // TODO: Implement module analysis
    }
    
    /// Analyze CONTAINS section
    fn analyze_contains_section(&mut self, contains: &crate::program::ContainsSection) {
        for internal_proc in &contains.internal_procedures {
            match internal_proc {
                crate::program::InternalProcedure::Subroutine(sub) => {
                    self.analyze_subroutine(sub);
                }
                crate::program::InternalProcedure::Function(func) => {
                    self.analyze_function(func);
                }
            }
        }
    }
    
    /// Analyze declaration
    fn analyze_declaration(&mut self, declaration: &Declaration) {
        use crate::analysis::{Symbol, SymbolType, SymbolMetadata};
        
        match declaration {
            Declaration::Variable { name, type_spec, .. } => {
                let symbol = Symbol {
                    name: name.clone(),
                    symbol_type: SymbolType::Variable {
                        data_type: format!("{:?}", type_spec), // TODO: proper type handling
                        dimensions: Vec::new(), // TODO: handle array dimensions
                    },
                    defined_at: crate::span::Span::new(0, 0, 1, 1), // TODO: proper span
                    used_at: Vec::new(),
                    metadata: SymbolMetadata::default(),
                };
                self.symbol_table_mut().define_symbol(symbol);
            }
            _ => {
                // TODO: Handle other declaration types
            }
        }
    }
    
    /// Analyze statement
    fn analyze_statement(&mut self, statement: &Statement) {
        match statement {
            Statement::Call { subroutine_name, arguments, .. } => {
                // Mark procedure call in call graph
                // TODO: Get current procedure name
                let current_proc = "current"; // TODO: track current procedure
                self.call_graph_mut().add_call(current_proc, subroutine_name);
                
                // Mark procedure as used
                self.symbol_table_mut().mark_used(
                    subroutine_name, 
                    crate::span::Span::new(0, 0, 1, 1) // TODO: proper span
                );
                
                // Mark arguments as used
                for arg in arguments {
                    self.analyze_expression(&arg.node);
                }
            }
            Statement::Assignment { variable, value, .. } => {
                // Mark variable as used/defined and analyze value
                self.analyze_expression(&variable.node);
                self.analyze_expression(&value.node);
            }
            _ => {
                // TODO: Handle other statements
            }
        }
    }
    
    /// Analyze expression
    fn analyze_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Variable(name) => {
                // Mark variable as used
                self.symbol_table_mut().mark_used(
                    name,
                    crate::span::Span::new(0, 0, 1, 1) // TODO: proper span
                );
            }
            Expression::ArrayElement { variable, subscripts } => {
                // Mark array variable as used
                self.symbol_table_mut().mark_used(
                    variable,
                    crate::span::Span::new(0, 0, 1, 1) // TODO: proper span
                );
                
                // Analyze subscripts
                for subscript in subscripts {
                    self.analyze_expression(&subscript.node);
                }
            }
            Expression::FunctionCall { name, arguments } => {
                // Mark function as used
                self.symbol_table_mut().mark_used(
                    name,
                    crate::span::Span::new(0, 0, 1, 1) // TODO: proper span
                );
                
                // Analyze arguments
                for arg in arguments {
                    self.analyze_expression(&arg.node);
                }
            }
            Expression::Binary { left, right, .. } => {
                self.analyze_expression(&left.node);
                self.analyze_expression(&right.node);
            }
            Expression::Unary { operand, .. } => {
                self.analyze_expression(&operand.node);
            }
            Expression::Literal(_) => {
                // Literals don't affect symbol usage
            }
            Expression::ArrayConstructor(elements) => {
                // Analyze array constructor elements
                for element in elements {
                    self.analyze_expression(&element.node);
                }
            }
        }
    }
}

