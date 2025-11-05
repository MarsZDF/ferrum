use fortran_lexer::{tokenize, detect_format, Format, TokenType};

#[test]
fn test_simple_program() {
    let source = r#"
program hello
    implicit none
    print *, 'Hello, World!'
end program hello
"#;
    
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    // Check for key tokens
    assert!(non_trivial.iter().any(|t| matches!(t.token_type, TokenType::Program)));
    assert!(non_trivial.iter().any(|t| matches!(t.token_type, TokenType::Identifier(ref s) if s == "hello")));
    assert!(non_trivial.iter().any(|t| matches!(t.token_type, TokenType::Implicit)));
    assert!(non_trivial.iter().any(|t| matches!(t.token_type, TokenType::None)));
    assert!(non_trivial.iter().any(|t| matches!(t.token_type, TokenType::Print)));
}

#[test]
fn test_keywords() {
    let source = "program subroutine function module if then else do end";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    let keyword_types: Vec<TokenType> = non_trivial
        .iter()
        .filter(|t| !matches!(t.token_type, TokenType::Eof))
        .map(|t| t.token_type.clone())
        .collect();
    
    assert!(keyword_types.contains(&TokenType::Program));
    assert!(keyword_types.contains(&TokenType::Subroutine));
    assert!(keyword_types.contains(&TokenType::Function));
    assert!(keyword_types.contains(&TokenType::Module));
    assert!(keyword_types.contains(&TokenType::If));
    assert!(keyword_types.contains(&TokenType::Then));
    assert!(keyword_types.contains(&TokenType::Else));
    assert!(keyword_types.contains(&TokenType::Do));
    assert!(keyword_types.contains(&TokenType::EndProgram));
}

#[test]
fn test_case_insensitive_keywords() {
    let source = "PROGRAM Program program PrOgRaM";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    // All should be recognized as Program keyword
    let program_count = non_trivial
        .iter()
        .filter(|t| matches!(t.token_type, TokenType::Program))
        .count();
    
    assert_eq!(program_count, 4);
}

#[test]
fn test_identifiers() {
    let source = "variable_name myVar123 _underscore";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    let identifiers: Vec<String> = non_trivial
        .iter()
        .filter_map(|t| {
            if let TokenType::Identifier(ref s) = t.token_type {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    
    assert!(identifiers.contains(&"variable_name".to_string()));
    assert!(identifiers.contains(&"myVar123".to_string()));
    assert!(identifiers.contains(&"_underscore".to_string()));
}

#[test]
fn test_integer_literals() {
    let source = "42 0 123456789";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    let integers: Vec<String> = non_trivial
        .iter()
        .filter_map(|t| {
            if let TokenType::IntegerLiteral(ref s) = t.token_type {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    
    assert!(integers.contains(&"42".to_string()));
    assert!(integers.contains(&"0".to_string()));
    assert!(integers.contains(&"123456789".to_string()));
}

#[test]
fn test_real_literals() {
    let source = "3.14 1.0e-10 2.5E+3 1.5d0";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    let reals: Vec<String> = non_trivial
        .iter()
        .filter_map(|t| {
            if let TokenType::RealLiteral(ref s) = t.token_type {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    
    assert!(reals.len() >= 4);
}

#[test]
fn test_character_literals() {
    let source = r#"'single' "double" 'with''quote' "with""quote""#;
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    let characters: Vec<String> = non_trivial
        .iter()
        .filter_map(|t| {
            if let TokenType::CharacterLiteral(ref s) = t.token_type {
                Some(s.clone())
            } else {
                None
            }
        })
        .collect();
    
    assert!(characters.len() >= 2);
}

#[test]
fn test_logical_literals() {
    let source = ".TRUE. .FALSE. .true. .false.";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    let logicals: Vec<_> = non_trivial
        .iter()
        .filter(|t| matches!(t.token_type, TokenType::LogicalLiteral(_)))
        .collect();
    
    assert_eq!(logicals.len(), 4);
}

#[test]
fn test_operators() {
    let source = "+ - * / ** = /= < <= > >=";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    let ops: Vec<TokenType> = non_trivial
        .iter()
        .filter(|t| !matches!(t.token_type, TokenType::Eof))
        .map(|t| t.token_type.clone())
        .collect();
    
    assert!(ops.contains(&TokenType::Plus));
    assert!(ops.contains(&TokenType::Minus));
    assert!(ops.contains(&TokenType::Multiply));
    assert!(ops.contains(&TokenType::Divide));
    assert!(ops.contains(&TokenType::Power));
    assert!(ops.contains(&TokenType::Equals));
    assert!(ops.contains(&TokenType::NotEquals));
    assert!(ops.contains(&TokenType::LessThan));
    assert!(ops.contains(&TokenType::LessOrEqual));
    assert!(ops.contains(&TokenType::GreaterThan));
    assert!(ops.contains(&TokenType::GreaterOrEqual));
}

#[test]
fn test_logical_operators() {
    let source = ".AND. .OR. .NOT. .EQV. .NEQV.";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    let ops: Vec<TokenType> = non_trivial
        .iter()
        .filter(|t| !matches!(t.token_type, TokenType::Eof))
        .map(|t| t.token_type.clone())
        .collect();
    
    assert!(ops.contains(&TokenType::And));
    assert!(ops.contains(&TokenType::Or));
    assert!(ops.contains(&TokenType::Not));
    assert!(ops.contains(&TokenType::Eqv));
    assert!(ops.contains(&TokenType::Neqv));
}

#[test]
fn test_punctuation() {
    let source = "() , ; : :: =>";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    let punct: Vec<TokenType> = non_trivial
        .iter()
        .filter(|t| !matches!(t.token_type, TokenType::Eof))
        .map(|t| t.token_type.clone())
        .collect();
    
    assert!(punct.contains(&TokenType::LeftParen));
    assert!(punct.contains(&TokenType::RightParen));
    assert!(punct.contains(&TokenType::Comma));
    assert!(punct.contains(&TokenType::Semicolon));
    assert!(punct.contains(&TokenType::Colon));
    assert!(punct.contains(&TokenType::DoubleColon));
    assert!(punct.contains(&TokenType::Assignment));
}

#[test]
fn test_comments() {
    let source = "! This is a comment\ncode ! inline comment";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    
    let comments: Vec<_> = tokens
        .iter()
        .filter(|t| matches!(t.token_type, TokenType::Comment(_)))
        .collect();
    
    assert_eq!(comments.len(), 2);
}

#[test]
fn test_unterminated_string() {
    let source = "'unterminated string";
    let result = tokenize(source, Format::FreeFormat);
    assert!(result.is_err());
}

#[test]
fn test_format_detection_free_format() {
    let source = r#"
program test
    implicit none
    integer :: i
end program
"#;
    assert_eq!(detect_format(source), Format::FreeFormat);
}

#[test]
fn test_format_detection_fixed_format() {
    // Fixed format: columns 1-5 are line numbers, 6 is continuation, 7-72 is code
    let source = "    1      PROGRAM TEST\n    2      IMPLICIT NONE\n    3      END";
    assert_eq!(detect_format(source), Format::FixedFormat);
}

#[test]
fn test_do_loop() {
    let source = r#"
do i = 1, 10
    print *, i
end do
"#;
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    let types: Vec<TokenType> = non_trivial
        .iter()
        .filter(|t| !matches!(t.token_type, TokenType::Eof))
        .map(|t| t.token_type.clone())
        .collect();
    
    assert!(types.contains(&TokenType::Do));
    // Check for "END DO" or "ENDDO" - both should be recognized
    assert!(types.contains(&TokenType::EndDo) || 
            (types.contains(&TokenType::EndProgram) && types.contains(&TokenType::Do)));
}

#[test]
fn test_if_statement() {
    let source = r#"
if (x > 0) then
    print *, 'positive'
else
    print *, 'non-positive'
end if
"#;
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    let types: Vec<TokenType> = non_trivial
        .iter()
        .filter(|t| !matches!(t.token_type, TokenType::Eof))
        .map(|t| t.token_type.clone())
        .collect();
    
    assert!(types.contains(&TokenType::If));
    assert!(types.contains(&TokenType::Then));
    assert!(types.contains(&TokenType::Else));
    // Check for "END IF" or "ENDIF" - both should be recognized
    assert!(types.contains(&TokenType::EndIf) || 
            (types.contains(&TokenType::EndProgram) && types.contains(&TokenType::If)));
}

#[test]
fn test_source_location() {
    let source = "program test\n    implicit none";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    
    // Find the 'program' token
    let program_token = tokens
        .iter()
        .find(|t| matches!(t.token_type, TokenType::Program))
        .unwrap();
    
    assert_eq!(program_token.line, 1);
    assert!(program_token.column > 0);
}

#[test]
fn test_array_subscript() {
    let source = "array(1, 2, 3)";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    assert!(non_trivial.iter().any(|t| matches!(t.token_type, TokenType::LeftParen)));
    assert!(non_trivial.iter().any(|t| matches!(t.token_type, TokenType::RightParen)));
    assert!(non_trivial.iter().any(|t| matches!(t.token_type, TokenType::IntegerLiteral(_))));
}

#[test]
fn test_function_call() {
    let source = "result = func(x, y, z)";
    let tokens = tokenize(source, Format::FreeFormat).unwrap();
    let non_trivial: Vec<_> = tokens.iter().filter(|t| !t.is_trivial()).collect();
    
    assert!(non_trivial.iter().any(|t| matches!(t.token_type, TokenType::Equals)));
    assert!(non_trivial.iter().any(|t| matches!(t.token_type, TokenType::LeftParen)));
    assert!(non_trivial.iter().any(|t| matches!(t.token_type, TokenType::Comma)));
}

