use std::env;
use std::fs;
use fortran_parser::parse;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <fortran-file>", args[0]);
        std::process::exit(1);
    }
    
    let file_path = &args[1];
    let source = fs::read_to_string(file_path)?;
    
    println!("=== FERRUM SIGNATURE EXTRACTOR ===");
    println!("Extracting modern interface from: {}", file_path);
    
    match parse(&source) {
        Ok(program) => {
            match &program.program_unit {
                fortran_ast::ProgramUnit::Subroutine(sub) => {
                    println!("\n// Extracted using Ferrum FORTRAN parser");
                    println!("// Original FORTRAN 77 signature:");
                    print!("//   SUBROUTINE {}(", sub.name);
                    for (i, arg) in sub.arguments.iter().enumerate() {
                        if i > 0 { print!(", "); }
                        print!("{}", arg.name);
                    }
                    println!(")");
                    
                    println!("\n// Modern Rust equivalent:");
                    println!("pub fn {}(", sub.name.to_lowercase());
                    
                    // Map FORTRAN parameters to modern types
                    for (i, arg) in sub.arguments.iter().enumerate() {
                        let rust_type = match arg.name.as_str() {
                            "MODE" => "&mut i32",
                            "M" | "N" | "LDFJAC" | "LWK" | "LIWK" => "usize",
                            "Y" | "F" | "YSCALE" | "FAC" => "&[f64]", 
                            "FJAC" => "&mut [f64]",
                            "WK" => "&mut [f64]",
                            "IWK" => "&mut [i32]",
                            "IOPT" => "&[i32]",
                            _ => "&[f64]"
                        };
                        
                        println!("    {}: {},", arg.name.to_lowercase(), rust_type);
                    }
                    println!(") -> Result<(), JacobianError> {{");
                    println!("    // Implementation here");
                    println!("    Ok(())");
                    println!("}}");
                    
                    println!("\n// Modern Python equivalent:");
                    println!("def {}(", sub.name.to_lowercase());
                    
                    for (i, arg) in sub.arguments.iter().enumerate() {
                        let py_type = match arg.name.as_str() {
                            "MODE" => "mode: int",
                            "M" | "N" | "LDFJAC" | "LWK" | "LIWK" => &format!("{}: int", arg.name.to_lowercase()),
                            "Y" | "F" | "YSCALE" | "FAC" | "FJAC" | "WK" => &format!("{}: np.ndarray", arg.name.to_lowercase()),
                            "IWK" | "IOPT" => &format!("{}: np.ndarray", arg.name.to_lowercase()),
                            _ => &format!("{}: np.ndarray", arg.name.to_lowercase())
                        };
                        
                        if i == sub.arguments.len() - 1 {
                            println!("    {}", py_type);
                        } else {
                            println!("    {},", py_type);
                        }
                    }
                    println!(") -> JacobianResult:");
                    println!("    \"\"\"Compute numerical Jacobian using finite differences.\"\"\"");
                    println!("    # Implementation here");
                    println!("    pass");
                }
                _ => println!("Not a subroutine"),
            }
        }
        Err(e) => {
            eprintln!("Failed to parse: {:?}", e);
            std::process::exit(1);
        }
    }
    
    Ok(())
}