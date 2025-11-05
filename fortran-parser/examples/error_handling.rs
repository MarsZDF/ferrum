use fortran_parser::{parse, ParseError};

fn main() {
    println!("Demonstrating error handling in FORTRAN parser...\n");
    
    // Example 1: Syntax error - missing THEN
    let invalid_syntax = r#"
program test
    implicit none
    integer :: x
    x = 5
    if (x > 0)
        print *, 'positive'
    end if
end program test
"#;
    
    println!("=== Example 1: Missing THEN keyword ===");
    println!("Source: {}", invalid_syntax);
    match parse(invalid_syntax) {
        Ok(_) => println!("✅ Parsed successfully (unexpected!)"),
        Err(ParseError::UnexpectedToken { expected, found }) => {
            println!("❌ Parse error: Expected {:?}, found {:?}", expected, found);
            println!("   Location: line {}, column {}", found.line, found.column);
        }
        Err(err) => println!("❌ Parse error: {:?}", err),
    }
    
    // Example 2: Lexer error - unterminated string
    let unterminated_string = r#"
program test
    print *, 'Hello world
end program test
"#;
    
    println!("\n=== Example 2: Unterminated string ===");
    println!("Source: {}", unterminated_string);
    match parse(unterminated_string) {
        Ok(_) => println!("✅ Parsed successfully (unexpected!)"),
        Err(err) => println!("❌ Parse error: {:?}", err),
    }
    
    // Example 3: Valid FORTRAN for comparison
    let valid_fortran = r#"
program factorial
    implicit none
    integer :: n, result, i
    
    print *, 'Enter a number:'
    read *, n
    
    result = 1
    do i = 1, n
        result = result * i
    end do
    
    print *, 'Factorial of', n, 'is', result
end program factorial
"#;
    
    println!("\n=== Example 3: Valid FORTRAN (for comparison) ===");
    println!("Source: {}", valid_fortran);
    match parse(valid_fortran) {
        Ok(program) => {
            println!("✅ Parsed successfully!");
            match &program.program_unit {
                fortran_ast::ProgramUnit::MainProgram(mp) => {
                    println!("   Program name: {:?}", mp.name);
                    println!("   Declarations: {}", mp.declarations.len());
                    println!("   Statements: {}", mp.executable_statements.len());
                }
                _ => println!("   Non-main program unit"),
            }
        }
        Err(err) => println!("❌ Parse error: {:?}", err),
    }
    
    println!("\n=== Error Recovery Tips ===");
    println!("1. Check token expectations in error messages");
    println!("2. Verify FORTRAN syntax (THEN after IF, END statements, etc.)");
    println!("3. Check for unterminated strings or comments");
    println!("4. Ensure proper keyword spelling and case");
    println!("5. Use the command-line parser for more detailed output");
}