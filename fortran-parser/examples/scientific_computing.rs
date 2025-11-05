use fortran_parser::parse;
use fortran_lexer::{tokenize, detect_format};
use fortran_ast::{ProgramUnit, Statement, Declaration};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Scientific Computing FORTRAN Examples ===\n");
    
    // Example 1: Numerical integration (simplified for parser)
    let integration_code = r#"
program numerical_integration
    implicit none
    real :: a, b, h, sum, x
    integer :: n, i
    
    a = 0.0
    b = 1.0
    n = 1000
    sum = 0.0
    
    do i = 1, n
        x = a + i * h
        sum = sum + x * x
    end do
    
    print *, 'Integral approximation:', sum
end program numerical_integration
"#;
    
    // Example 2: Matrix operations (simplified)
    let matrix_code = r#"
program matrix_operations
    implicit none
    integer :: n
    real :: sum
    integer :: i, j
    
    n = 3
    sum = 0.0
    
    do i = 1, n
        do j = 1, n
            sum = sum + i * j
        end do
    end do
    
    print *, 'Sum:', sum
end program matrix_operations
"#;
    
    // Parse and analyze the integration code
    println!("=== Analyzing Numerical Integration Code ===");
    analyze_scientific_code("numerical_integration", integration_code)?;
    
    println!("\n=== Analyzing Matrix Operations Code ===");
    analyze_scientific_code("matrix_operations", matrix_code)?;
    
    Ok(())
}

fn analyze_scientific_code(name: &str, source: &str) -> Result<(), Box<dyn std::error::Error>> {
    println!("Parsing {} code...", name);
    
    // Detect format
    let format = detect_format(source);
    println!("Detected format: {:?}", format);
    
    // Tokenize
    let tokens = tokenize(source, format)?;
    let non_trivial_tokens: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    println!("Tokens: {} total, {} non-trivial", tokens.len(), non_trivial_tokens.len());
    
    // Parse
    let program = parse(source)?;
    println!("✅ Parsed successfully!");
    
    // Analyze program structure
    match &program.program_unit {
        ProgramUnit::MainProgram(mp) => {
            println!("Main Program Analysis:");
            println!("  Name: {:?}", mp.name);
            println!("  Declarations: {}", mp.declarations.len());
            println!("  Executable statements: {}", mp.executable_statements.len());
            
            // Count different types of statements
            let mut assignments = 0;
            let mut loops = 0;
            let mut conditionals = 0;
            let mut io_statements = 0;
            
            for stmt in &mp.executable_statements {
                match &stmt.node {
                    Statement::Assignment { .. } => assignments += 1,
                    Statement::Do { .. } | Statement::DoWhile { .. } => loops += 1,
                    Statement::If { .. } => conditionals += 1,
                    Statement::Read { .. } | Statement::Write { .. } | Statement::Print { .. } => io_statements += 1,
                    _ => {}
                }
            }
            
            println!("  Statement breakdown:");
            println!("    Assignments: {}", assignments);
            println!("    Loops: {}", loops);
            println!("    Conditionals: {}", conditionals);
            println!("    I/O statements: {}", io_statements);
            
            // Analyze declarations
            let mut variable_count = 0;
            let mut parameter_count = 0;
            
            for decl in &mp.declarations {
                match &decl.node {
                    Declaration::Variable { .. } => variable_count += 1,
                    Declaration::Parameter { .. } => parameter_count += 1,
                    _ => {}
                }
            }
            
            println!("  Declaration breakdown:");
            println!("    Variables: {}", variable_count);
            println!("    Parameters: {}", parameter_count);
        }
        ProgramUnit::Function(func) => {
            println!("Function: {}", func.name);
            println!("  Arguments: {}", func.arguments.len());
            println!("  Result: {:?}", func.result_name);
        }
        ProgramUnit::Subroutine(sub) => {
            println!("Subroutine: {}", sub.name);
            println!("  Arguments: {}", sub.arguments.len());
        }
        ProgramUnit::Module(module) => {
            println!("Module: {}", module.name);
        }
    }
    
    Ok(())
}