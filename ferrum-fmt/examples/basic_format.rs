use ferrum_fmt::{format_source, FormatConfig, KeywordCase};
use fortran_lexer::detect_format;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
program hello_world
    implicit none
    integer :: x, y, z
    x = 10
    y = 20
    z = x + y
    print *, 'Sum:', z
end program hello_world
"#;
    
    println!("Original code:");
    println!("{}", source);
    println!("\n{}", "=".repeat(50));
    println!("\nFormatted with default config:\n");
    
    let format = detect_format(source);
    let formatted = format_source(source, format, FormatConfig::default())?;
    println!("{}", formatted);
    
    println!("\n{}", "=".repeat(50));
    println!("\nFormatted with FORTRAN 77 style:\n");
    
    let formatted_77 = format_source(source, format, FormatConfig::fortran77())?;
    println!("{}", formatted_77);
    
    println!("\n{}", "=".repeat(50));
    println!("\nFormatted with modern style:\n");
    
    let formatted_modern = format_source(source, format, FormatConfig::modern())?;
    println!("{}", formatted_modern);
    
    Ok(())
}

