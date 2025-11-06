use std::env;
use std::fs;
use std::path::Path;
use fortran_parser::parse;
use fortran_lexer::detect_format;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: {} <fortran-file>", args.first().unwrap_or(&"fortran-parser".to_string()));
        eprintln!("\nExample:");
        eprintln!("  {} djacg.f", args.first().unwrap_or(&"fortran-parser".to_string()));
        std::process::exit(1);
    }
    
    let file_path = &args[1];
    
    // Check if file exists
    if !Path::new(file_path).exists() {
        eprintln!("Error: File '{}' not found", file_path);
        std::process::exit(1);
    }
    
    // Read file contents
    println!("Reading FORTRAN file: {}", file_path);
    let source = fs::read_to_string(file_path)?;
    
    // Detect format (fixed or free)
    let format = detect_format(&source);
    println!("Detected format: {:?}", format);
    
    if format == fortran_lexer::Format::FixedFormat {
        println!("✅ Fixed-format FORTRAN detected - using fixed-format lexer!");
    }
    
    // Parse the source
    println!("\nParsing FORTRAN source...\n");
    match parse(&source) {
        Ok(program) => {
            println!("✅ Successfully parsed program!\n");
            
            // Print program information
            match &program.program_unit {
                fortran_ast::ProgramUnit::MainProgram(mp) => {
                    if let Some(name) = &mp.name {
                        println!("Program name: {}", name);
                    } else {
                        println!("Program name: (unnamed)");
                    }
                    println!("Declarations: {}", mp.declarations.len());
                    println!("Executable statements: {}", mp.executable_statements.len());
                }
                fortran_ast::ProgramUnit::Subroutine(sub) => {
                    println!("Subroutine: {}", sub.name);
                    println!("Arguments: {}", sub.arguments.len());
                    println!("Declarations: {}", sub.declarations.len());
                    println!("Executable statements: {}", sub.executable_statements.len());
                }
                fortran_ast::ProgramUnit::Function(func) => {
                    println!("Function: {}", func.name);
                    println!("Arguments: {}", func.arguments.len());
                    if let Some(result_name) = &func.result_name {
                        println!("Result name: {}", result_name);
                    }
                    println!("Declarations: {}", func.declarations.len());
                    println!("Executable statements: {}", func.executable_statements.len());
                }
                fortran_ast::ProgramUnit::Module(module) => {
                    println!("Module: {}", module.name);
                    println!("Declarations: {}", module.declarations.len());
                }
            }
            
            // Optionally print full AST (verbose mode)
            if args.contains(&"--verbose".to_string()) || args.contains(&"-v".to_string()) {
                println!("\n=== Full AST ===");
                println!("{:#?}", program);
            }
            
            Ok(())
        }
        Err(e) => {
            eprintln!("❌ Failed to parse: {:?}", e);
            std::process::exit(1);
        }
    }
}

