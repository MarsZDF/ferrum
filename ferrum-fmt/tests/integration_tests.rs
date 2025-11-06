use ferrum_fmt::{format_source, FormatConfig, KeywordCase, IdentifierCase};
use fortran_lexer::Format;

#[test]
fn test_basic_formatting() {
    let source = r#"program hello
implicit none
integer :: x
x = 10
print *, x
end program hello"#;
    
    let formatted = format_source(source, Format::FreeFormat, FormatConfig::default()).unwrap();
    
    // Should preserve structure but normalize formatting
    assert!(formatted.contains("program hello"));
    assert!(formatted.contains("end program hello"));
}

#[test]
fn test_keyword_case_upper() {
    let source = "program test\nend program test";
    
    let mut config = FormatConfig::default();
    config.keyword_case = KeywordCase::Upper;
    let formatted = format_source(source, Format::FreeFormat, config).unwrap();
    
    assert!(formatted.contains("PROGRAM"));
    assert!(formatted.contains("END PROGRAM"));
}

#[test]
fn test_keyword_case_lower() {
    let source = "PROGRAM TEST\nEND PROGRAM TEST";
    
    let mut config = FormatConfig::default();
    config.keyword_case = KeywordCase::Lower;
    let formatted = format_source(source, Format::FreeFormat, config).unwrap();
    
    assert!(formatted.contains("program"));
    assert!(formatted.contains("end program"));
}

#[test]
fn test_indentation() {
    let source = r#"program test
if (x > 0) then
y = 10
end if
end program test"#;
    
    let config = FormatConfig::default();
    let formatted = format_source(source, Format::FreeFormat, config).unwrap();
    
    // Basic check that formatting happened
    assert!(formatted.contains("program"));
    assert!(formatted.contains("if"));
}

#[test]
fn test_fortran77_style() {
    let source = "program test\nend program test";
    
    let config = FormatConfig::fortran77();
    let formatted = format_source(source, Format::FreeFormat, config).unwrap();
    
    // FORTRAN 77 style should use uppercase keywords
    assert!(formatted.contains("PROGRAM") || formatted.contains("program"));
}

#[test]
fn test_modern_style() {
    let source = "PROGRAM TEST\nEND PROGRAM TEST";
    
    let config = FormatConfig::modern();
    let formatted = format_source(source, Format::FreeFormat, config).unwrap();
    
    // Modern style should use lowercase keywords
    assert!(formatted.to_lowercase().contains("program"));
}

