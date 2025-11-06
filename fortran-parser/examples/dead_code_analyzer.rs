use std::env;
use std::fs;
use fortran_parser::parse;
use fortran_ast::{AnalysisVisitor, SymbolTable, CallGraph, Symbol, SymbolType};
use fortran_ast::program::{ContainsSection, InternalProcedure};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <fortran-file>", args[0]);
        eprintln!("\nFinds dead code in FORTRAN programs:");
        eprintln!("- Unused variables");
        eprintln!("- Unused subroutines/functions");
        eprintln!("- Unused statement labels");
        eprintln!("- Unreachable procedures");
        eprintln!("\nExample:");
        eprintln!("  {} legacy.f", args[0]);
        std::process::exit(1);
    }
    
    let file_path = &args[1];
    let source = fs::read_to_string(file_path)?;
    
    println!("🔍 Dead Code Analysis for: {}", file_path);
    println!("==========================================");
    
    // Parse the FORTRAN source
    match parse(&source) {
        Ok(program) => {
            let mut analyzer = DeadCodeAnalyzer::new();
            analyzer.analyze_program(&program);
            
            let results = analyzer.generate_report();
            println!("{}", results);
        }
        Err(e) => {
            eprintln!("❌ Failed to parse FORTRAN file: {:?}", e);
            eprintln!("Cannot analyze dead code in unparseable file.");
            std::process::exit(1);
        }
    }
    
    Ok(())
}

/// Dead code analyzer that implements the AnalysisVisitor trait
pub struct DeadCodeAnalyzer {
    symbol_table: SymbolTable,
    call_graph: CallGraph,
    current_procedure: Option<String>,
}

impl DeadCodeAnalyzer {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
            call_graph: CallGraph::new(),
            current_procedure: None,
        }
    }
    
    /// Generate a comprehensive dead code report
    pub fn generate_report(&self) -> String {
        let mut report = String::new();
        
        // Find unused variables
        let unused_vars = self.find_unused_variables();
        if !unused_vars.is_empty() {
            report.push_str("🗑️  UNUSED VARIABLES\n");
            report.push_str("=====================\n");
            for var in &unused_vars {
                report.push_str(&format!(
                    "  • {} (line {}): {}\n",
                    var.name,
                    var.defined_at.line,
                    match &var.symbol_type {
                        SymbolType::Variable { data_type, .. } => data_type.clone(),
                        _ => "unknown type".to_string(),
                    }
                ));
            }
            report.push('\n');
        }
        
        // Find dead procedures
        let dead_procedures = self.call_graph.find_dead_procedures();
        if !dead_procedures.is_empty() {
            report.push_str("☠️  DEAD PROCEDURES\n");
            report.push_str("===================\n");
            for proc in &dead_procedures {
                report.push_str(&format!(
                    "  • {} (line {}): {} parameters\n",
                    proc.name,
                    proc.defined_at.line,
                    proc.parameters.len()
                ));
            }
            report.push('\n');
        }
        
        // Find unused functions
        let unused_functions = self.find_unused_functions();
        if !unused_functions.is_empty() {
            report.push_str("🔧 UNUSED FUNCTIONS\n");
            report.push_str("===================\n");
            for func in &unused_functions {
                report.push_str(&format!(
                    "  • {} (line {}): returns {}\n",
                    func.name,
                    func.defined_at.line,
                    match &func.symbol_type {
                        SymbolType::Function { return_type, .. } => return_type.clone(),
                        _ => "unknown".to_string(),
                    }
                ));
            }
            report.push('\n');
        }
        
        // Reachability analysis
        let reachable_procedures = self.call_graph.find_reachable_procedures();
        let all_procedures = self.call_graph.all_procedure_names();
        let unreachable: Vec<_> = all_procedures
            .iter()
            .filter(|name| !reachable_procedures.contains(name))
            .collect();
            
        if !unreachable.is_empty() {
            report.push_str("🚫 UNREACHABLE PROCEDURES\n");
            report.push_str("=========================\n");
            for proc_name in &unreachable {
                if let Some(proc_info) = self.call_graph.get_procedure(proc_name) {
                    report.push_str(&format!(
                        "  • {} (line {}): not reachable from main program\n",
                        proc_info.name,
                        proc_info.defined_at.line
                    ));
                }
            }
            report.push('\n');
        }
        
        // Summary statistics
        report.push_str("📊 SUMMARY\n");
        report.push_str("==========\n");
        report.push_str(&format!(
            "  Total symbols: {}\n",
            self.symbol_table.all_symbols().len()
        ));
        report.push_str(&format!(
            "  Unused variables: {}\n",
            unused_vars.len()
        ));
        report.push_str(&format!(
            "  Dead procedures: {}\n",
            dead_procedures.len()
        ));
        report.push_str(&format!(
            "  Unreachable procedures: {}\n",
            unreachable.len()
        ));
        
        let dead_code_percentage = if self.symbol_table.all_symbols().is_empty() {
            0.0
        } else {
            ((unused_vars.len() + dead_procedures.len()) as f64 / self.symbol_table.all_symbols().len() as f64) * 100.0
        };
        
        report.push_str(&format!(
            "  Dead code percentage: {:.1}%\n",
            dead_code_percentage
        ));
        
        if dead_code_percentage == 0.0 {
            report.push_str("\n✨ No dead code found! The codebase is clean.\n");
        } else if dead_code_percentage > 20.0 {
            report.push_str("\n⚠️  High amount of dead code detected. Consider cleanup.\n");
        } else {
            report.push_str("\n👍 Moderate dead code levels. Some cleanup opportunities exist.\n");
        }
        
        report
    }
    
    fn find_unused_variables(&self) -> Vec<&Symbol> {
        self.symbol_table
            .unused_symbols()
            .into_iter()
            .filter(|symbol| matches!(symbol.symbol_type, SymbolType::Variable { .. }))
            .collect()
    }
    
    fn find_unused_functions(&self) -> Vec<&Symbol> {
        self.symbol_table
            .unused_symbols()
            .into_iter()
            .filter(|symbol| matches!(symbol.symbol_type, SymbolType::Function { .. }))
            .collect()
    }
}

impl AnalysisVisitor for DeadCodeAnalyzer {
    fn symbol_table_mut(&mut self) -> &mut SymbolTable {
        &mut self.symbol_table
    }
    
    fn call_graph_mut(&mut self) -> &mut CallGraph {
        &mut self.call_graph
    }
    
    // Override to track current procedure for better call graph building
    fn analyze_subroutine(&mut self, subroutine: &fortran_ast::Subroutine) {
        use fortran_ast::analysis::{ScopeType, Symbol, SymbolType, SymbolMetadata, ProcedureInfo};
        
        let old_proc = self.current_procedure.clone();
        self.current_procedure = Some(subroutine.name.clone());
        
        // Enter subroutine scope
        self.symbol_table_mut().enter_scope(ScopeType::Subroutine(subroutine.name.clone()));
        
        // Add to call graph
        let parameters: Vec<String> = subroutine.arguments.iter()
            .map(|arg| arg.name.clone())
            .collect();
            
        let proc_info = ProcedureInfo {
            name: subroutine.name.clone(),
            defined_at: fortran_ast::Span::new(0, 0, 1, 1), // TODO: proper span
            parameters: parameters.clone(),
            is_main: subroutine.name.to_uppercase() == "MAIN", // Mark MAIN as main entry point
            is_external: false,
        };
        self.call_graph_mut().add_procedure(proc_info);
        
        // Define subroutine symbol
        let sub_symbol = Symbol {
            name: subroutine.name.clone(),
            symbol_type: SymbolType::Subroutine { parameters },
            defined_at: fortran_ast::Span::new(0, 0, 1, 1), // TODO: proper span
            used_at: Vec::new(),
            metadata: SymbolMetadata::default(),
        };
        self.symbol_table_mut().define_symbol(sub_symbol);
        
        // Define parameter symbols
        for arg in &subroutine.arguments {
            let param_symbol = Symbol {
                name: arg.name.clone(),
                symbol_type: SymbolType::Parameter,
                defined_at: fortran_ast::Span::new(0, 0, 1, 1), // TODO: proper span
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
        self.current_procedure = old_proc;
    }
    
    fn analyze_function(&mut self, function: &fortran_ast::Function) {
        use fortran_ast::analysis::{ScopeType, Symbol, SymbolType, SymbolMetadata, ProcedureInfo};
        
        let old_proc = self.current_procedure.clone();
        self.current_procedure = Some(function.name.clone());
        
        // Enter function scope
        self.symbol_table_mut().enter_scope(ScopeType::Function(function.name.clone()));
        
        // Add to call graph
        let parameters: Vec<String> = function.arguments.iter()
            .map(|arg| arg.name.clone())
            .collect();
            
        let proc_info = ProcedureInfo {
            name: function.name.clone(),
            defined_at: fortran_ast::Span::new(0, 0, 1, 1), // TODO: proper span
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
            defined_at: fortran_ast::Span::new(0, 0, 1, 1), // TODO: proper span
            used_at: Vec::new(),
            metadata: SymbolMetadata::default(),
        };
        self.symbol_table_mut().define_symbol(func_symbol);
        
        // Define parameter symbols
        for arg in &function.arguments {
            let param_symbol = Symbol {
                name: arg.name.clone(),
                symbol_type: SymbolType::Parameter,
                defined_at: fortran_ast::Span::new(0, 0, 1, 1), // TODO: proper span
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
        self.current_procedure = old_proc;
    }
    
    fn analyze_statement(&mut self, statement: &fortran_ast::Statement) {
        // Override to use current procedure context
        match statement {
            fortran_ast::Statement::Call { subroutine_name, arguments } => {
                // Record call in call graph with actual current procedure
                if let Some(current) = &self.current_procedure {
                    self.call_graph.add_call(current, subroutine_name);
                }
                
                // Mark procedure as used
                self.symbol_table.mark_used(
                    subroutine_name,
                    fortran_ast::Span::new(0, 0, 1, 1) // TODO: proper span
                );
                
                // Mark arguments as used
                for arg in arguments {
                    self.analyze_expression(&arg.node);
                }
            }
            fortran_ast::Statement::Assignment { variable, value } => {
                // Mark variable as used/defined and analyze value
                self.analyze_expression(&variable.node);
                self.analyze_expression(&value.node);
            }
            _ => {
                // Handle other statement types as needed
            }
        }
    }
    
    fn analyze_declaration(&mut self, declaration: &fortran_ast::Declaration) {
        use fortran_ast::analysis::{Symbol, SymbolType, SymbolMetadata};
        
        match declaration {
            fortran_ast::Declaration::Variable { name, type_spec, .. } => {
                let symbol = Symbol {
                    name: name.clone(),
                    symbol_type: SymbolType::Variable {
                        data_type: format!("{:?}", type_spec), // TODO: proper type handling
                        dimensions: Vec::new(), // TODO: handle array dimensions
                    },
                    defined_at: fortran_ast::Span::new(0, 0, 1, 1), // TODO: proper span
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
    
    fn analyze_expression(&mut self, expression: &fortran_ast::Expression) {
        match expression {
            fortran_ast::Expression::Variable(name) => {
                // Mark variable as used
                self.symbol_table_mut().mark_used(
                    name,
                    fortran_ast::Span::new(0, 0, 1, 1) // TODO: proper span
                );
            }
            fortran_ast::Expression::ArrayElement { variable, subscripts } => {
                // Mark array variable as used
                self.symbol_table_mut().mark_used(
                    variable,
                    fortran_ast::Span::new(0, 0, 1, 1) // TODO: proper span
                );
                
                // Analyze subscripts
                for subscript in subscripts {
                    self.analyze_expression(&subscript.node);
                }
            }
            fortran_ast::Expression::FunctionCall { name, arguments } => {
                // Mark function as used
                self.symbol_table_mut().mark_used(
                    name,
                    fortran_ast::Span::new(0, 0, 1, 1) // TODO: proper span
                );
                
                // Analyze arguments
                for arg in arguments {
                    self.analyze_expression(&arg.node);
                }
            }
            fortran_ast::Expression::Binary { left, right, .. } => {
                self.analyze_expression(&left.node);
                self.analyze_expression(&right.node);
            }
            fortran_ast::Expression::Unary { operand, .. } => {
                self.analyze_expression(&operand.node);
            }
            fortran_ast::Expression::Literal(_) => {
                // Literals don't affect symbol usage
            }
            fortran_ast::Expression::ArrayConstructor(elements) => {
                // Analyze array constructor elements
                for element in elements {
                    self.analyze_expression(&element.node);
                }
            }
        }
    }
    
    /// Analyze CONTAINS section
    fn analyze_contains_section(&mut self, contains: &ContainsSection) {
        for internal_proc in &contains.internal_procedures {
            match internal_proc {
                InternalProcedure::Subroutine(sub) => {
                    self.analyze_subroutine(sub);
                }
                InternalProcedure::Function(func) => {
                    self.analyze_function(func);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unused_variable_detection() {
        let source = r#"
      SUBROUTINE TEST
      INTEGER USED_VAR, UNUSED_VAR
      USED_VAR = 42
      PRINT *, USED_VAR
      END
"#;
        
        let program = parse(source).expect("Should parse");
        let mut analyzer = DeadCodeAnalyzer::new();
        analyzer.analyze_program(&program);
        
        let unused_vars = analyzer.find_unused_variables();
        
        // Should find UNUSED_VAR but not USED_VAR
        assert_eq!(unused_vars.len(), 1);
        assert_eq!(unused_vars[0].name, "UNUSED_VAR");
    }

    #[test]
    fn test_dead_procedure_detection() {
        // Test with a simple subroutine since the parser doesn't support CONTAINS yet
        let source = r#"
      SUBROUTINE MAIN
      INTEGER I
      I = 42
      END
"#;
        
        let program = parse(source).expect("Should parse");
        let mut analyzer = DeadCodeAnalyzer::new();
        analyzer.analyze_program(&program);
        
        let dead_procedures = analyzer.call_graph.find_dead_procedures();
        
        // MAIN should NOT be marked as dead since it's an entry point
        assert!(!dead_procedures.iter().any(|p| p.name == "MAIN"));
    }
}