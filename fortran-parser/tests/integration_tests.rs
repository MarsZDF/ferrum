use fortran_parser::parse;
use fortran_ast::{Program, ProgramUnit, Statement};

#[test]
fn test_parse_simple_program() {
    let source = r#"
program hello
    implicit none
    print *, 'Hello, World!'
end program hello
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let program = result.unwrap();
    match program.program_unit {
        ProgramUnit::MainProgram(mp) => {
            assert_eq!(mp.name, Some("hello".to_string()));
        }
        _ => panic!("Expected MainProgram"),
    }
}

#[test]
fn test_parse_variable_declarations() {
    let source = r#"
program test
    implicit none
    integer :: i, j, k
    real :: x, y
    character(len=10) :: name
end program test
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let program = result.unwrap();
    match program.program_unit {
        ProgramUnit::MainProgram(mp) => {
            assert!(!mp.declarations.is_empty(), "Should have declarations");
        }
        _ => panic!("Expected MainProgram"),
    }
}

#[test]
fn test_parse_if_statement() {
    let source = r#"
program test
    implicit none
    integer :: x
    if (x > 0) then
        print *, 'positive'
    else
        print *, 'non-positive'
    end if
end program test
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let program = result.unwrap();
    match program.program_unit {
        ProgramUnit::MainProgram(mp) => {
            assert!(!mp.executable_statements.is_empty(), "Should have statements");
            if let Statement::If { .. } = mp.executable_statements[0].node {
                // Good
            } else {
                panic!("Expected IF statement");
            }
        }
        _ => panic!("Expected MainProgram"),
    }
}

#[test]
fn test_parse_do_loop() {
    let source = r#"
program test
    implicit none
    integer :: i
    do i = 1, 10
        print *, i
    end do
end program test
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let program = result.unwrap();
    match program.program_unit {
        ProgramUnit::MainProgram(mp) => {
            assert!(!mp.executable_statements.is_empty(), "Should have statements");
            if let Statement::Do { .. } = mp.executable_statements[0].node {
                // Good
            } else {
                panic!("Expected DO statement");
            }
        }
        _ => panic!("Expected MainProgram"),
    }
}

#[test]
fn test_parse_assignment() {
    let source = r#"
program test
    implicit none
    integer :: x, y
    x = 42
    y = x + 1
end program test
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let program = result.unwrap();
    match program.program_unit {
        ProgramUnit::MainProgram(mp) => {
            assert!(!mp.executable_statements.is_empty(), "Should have statements");
            if let Statement::Assignment { .. } = mp.executable_statements[0].node {
                // Good
            } else {
                panic!("Expected assignment statement");
            }
        }
        _ => panic!("Expected MainProgram"),
    }
}

#[test]
fn test_parse_subroutine() {
    let source = r#"
subroutine test_sub(x, y)
    implicit none
    integer, intent(in) :: x
    integer, intent(out) :: y
    y = x * 2
end subroutine test_sub
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let program = result.unwrap();
    match program.program_unit {
        ProgramUnit::Subroutine(sub) => {
            assert_eq!(sub.name, "test_sub");
            assert!(!sub.arguments.is_empty(), "Should have arguments");
        }
        _ => panic!("Expected Subroutine"),
    }
}

#[test]
fn test_parse_function() {
    let source = r#"
function add(x, y) result(z)
    implicit none
    integer, intent(in) :: x, y
    integer :: z
    z = x + y
end function add
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let program = result.unwrap();
    match program.program_unit {
        ProgramUnit::Function(func) => {
            assert_eq!(func.name, "add");
            assert_eq!(func.result_name, Some("z".to_string()));
        }
        _ => panic!("Expected Function"),
    }
}

#[test]
fn test_parse_expressions() {
    let source = r#"
program test
    implicit none
    integer :: a, b, c
    a = 1 + 2 * 3
    b = (a + 1) ** 2
    c = a .gt. 5
end program test
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
}

#[test]
fn test_parse_read_write() {
    let source = r#"
program test
    implicit none
    integer :: x
    read *, x
    write(*,*) 'Value:', x
end program test
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let program = result.unwrap();
    match program.program_unit {
        ProgramUnit::MainProgram(mp) => {
            assert!(!mp.executable_statements.is_empty(), "Should have statements");
            // First should be READ
            if let Statement::Read { .. } = mp.executable_statements[0].node {
                // Good
            } else {
                panic!("Expected READ statement");
            }
        }
        _ => panic!("Expected MainProgram"),
    }
}

#[test]
fn test_parse_module() {
    let source = r#"
module my_module
    implicit none
    integer :: module_var
contains
    subroutine sub1()
        print *, 'sub1'
    end subroutine sub1
end module my_module
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let program = result.unwrap();
    match program.program_unit {
        ProgramUnit::Module(module) => {
            assert_eq!(module.name, "my_module");
            assert!(module.contains.is_some(), "Should have CONTAINS section");
        }
        _ => panic!("Expected Module"),
    }
}

#[test]
fn test_parse_complex_program() {
    let source = r#"
program complex_test
    implicit none
    integer :: i, j, sum
    real :: x(10), total
    
    sum = 0
    total = 0.0
    
    do i = 1, 10
        x(i) = real(i) * 2.5
        total = total + x(i)
        if (x(i) > 10.0) then
            sum = sum + 1
        end if
    end do
    
    print *, 'Total:', total
    print *, 'Count > 10:', sum
end program complex_test
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
}

#[test]
fn test_parse_select_case() {
    let source = r#"
program test
    implicit none
    integer :: x
    select case (x)
    case (1)
        print *, 'One'
    case (2:5)
        print *, 'Two to five'
    case default
        print *, 'Other'
    end select
end program test
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let program = result.unwrap();
    match program.program_unit {
        ProgramUnit::MainProgram(mp) => {
            assert!(!mp.executable_statements.is_empty(), "Should have statements");
            if let Statement::SelectCase { .. } = mp.executable_statements[0].node {
                // Good
            } else {
                panic!("Expected SELECT CASE statement");
            }
        }
        _ => panic!("Expected MainProgram"),
    }
}

#[test]
fn test_parse_do_while() {
    let source = r#"
program test
    implicit none
    integer :: i
    i = 0
    do while (i < 10)
        i = i + 1
        print *, i
    end do
end program test
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let program = result.unwrap();
    match program.program_unit {
        ProgramUnit::MainProgram(mp) => {
            assert!(!mp.executable_statements.is_empty(), "Should have statements");
            // Find DO WHILE statement
            let found_do_while = mp.executable_statements.iter().any(|s| {
                matches!(s.node, Statement::DoWhile { .. })
            });
            assert!(found_do_while, "Should have DO WHILE statement");
        }
        _ => panic!("Expected MainProgram"),
    }
}

#[test]
fn test_parse_array_access() {
    let source = r#"
program test
    implicit none
    integer :: arr(10)
    arr(1) = 42
    arr(2) = arr(1) + 1
end program test
"#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
}

#[test]
fn test_parse_error_handling() {
    // Test with invalid syntax - missing END
    let source = r#"
program test
    implicit none
    integer :: x
    x = 42
"#;
    
    let result = parse(source);
    // This might parse or error - both are acceptable
    // The important thing is it doesn't panic
    let _ = result;
}

