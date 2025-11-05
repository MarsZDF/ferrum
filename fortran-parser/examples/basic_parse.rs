use fortran_parser::parse;

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
    
    print *, 'Hello, World!'
end program hello_world
"#;

    println!("Parsing FORTRAN source...\n");
    
    let program = parse(source)?;
    
    println!("Successfully parsed program!");
    println!("Program unit: {:?}", program.program_unit);
    
    Ok(())
}

