use fortran_lexer::{tokenize, Format};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
program hello_world
    implicit none
    integer :: i
    real :: x
    
    x = 3.14159
    do i = 1, 10
        if (i .gt. 5) then
            print *, 'Greater than 5: ', i
        else
            print *, 'Less than or equal to 5: ', i
        end if
    end do
    
    write(*,*) 'Hello, World!'
end program hello_world
"#;

    println!("Tokenizing FORTRAN source...\n");
    
    let tokens = tokenize(source, Format::FreeFormat)?;
    
    println!("Found {} tokens:\n", tokens.len());
    
    for token in tokens {
        if !token.is_trivial() {
            println!("{:?} at line {}:{}", token.token_type, token.line, token.column);
        }
    }
    
    Ok(())
}

