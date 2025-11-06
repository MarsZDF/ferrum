use std::env;
use std::fs;
use fortran_lexer::{detect_format, Format};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <fixed-format-fortran-file> [output-file]", args[0]);
        eprintln!("\nConverts fixed-format FORTRAN 77 to modern free-format.");
        eprintln!("\nExample:");
        eprintln!("  {} legacy.f modern.f90", args[0]);
        eprintln!("  {} legacy.f  # outputs to stdout", args[0]);
        std::process::exit(1);
    }
    
    let input_file = &args[1];
    let output_file = args.get(2);
    
    let source = fs::read_to_string(input_file)?;
    
    // Verify it's actually fixed-format
    let format = detect_format(&source);
    if format != Format::FixedFormat {
        eprintln!("Warning: File doesn't appear to be fixed-format FORTRAN");
        eprintln!("Detected format: {:?}", format);
        eprintln!("Continuing anyway...");
    }
    
    println!("Converting fixed-format FORTRAN to free-format...");
    
    // Convert using Ferrum's lexer and reconstruction
    let converted = convert_fixed_to_free(&source)?;
    
    match output_file {
        Some(output_path) => {
            fs::write(output_path, &converted)?;
            println!("✅ Converted {} -> {}", input_file, output_path);
        }
        None => {
            println!("\n=== CONVERTED FREE-FORMAT FORTRAN ===");
            println!("{}", converted);
        }
    }
    
    Ok(())
}

fn convert_fixed_to_free(source: &str) -> Result<String, Box<dyn std::error::Error>> {
    let lines: Vec<&str> = source.lines().collect();
    let mut free_format_lines = Vec::new();
    let mut in_continuation = false;
    
    for (_line_num, line) in lines.iter().enumerate() {
        
        // Handle empty lines
        if line.trim().is_empty() {
            free_format_lines.push(String::new());
            continue;
        }
        
        // Check for comment lines (C, c, *, ! in column 1)
        if let Some(first_char) = line.chars().next() {
            if matches!(first_char, 'C' | 'c' | '*') {
                // Convert old-style comments to modern ! comments
                let comment_content = line.trim_start_matches(['C', 'c', '*']).trim_start();
                free_format_lines.push(format!("! {}", comment_content));
                continue;
            } else if first_char == '!' {
                // Already modern comment, keep as-is
                free_format_lines.push(line.to_string());
                continue;
            }
        }
        
        // Process fixed-format statement line
        let chars: Vec<char> = line.chars().collect();
        
        // Extract parts according to fixed-format rules
        let label_part = if chars.len() >= 5 {
            chars[0..5].iter().collect::<String>().trim().to_string()
        } else {
            String::new()
        };
        
        let continuation_char = if chars.len() >= 6 {
            chars[5]
        } else {
            ' '
        };
        
        let is_continuation = continuation_char != ' ' && continuation_char != '0';
        
        // Extract code part (columns 7-72)
        let code_start = 6; // 0-indexed, so column 7
        let code_end = if chars.len() > 72 { 72 } else { chars.len() };
        
        let code_part = if code_start < chars.len() {
            chars[code_start..code_end].iter().collect::<String>()
        } else {
            String::new()
        };
        
        // Build free-format line
        let mut free_line = String::new();
        
        // Add label if present
        if !label_part.is_empty() && label_part.chars().all(|c| c.is_ascii_digit()) {
            free_line.push_str(&label_part);
            free_line.push(' ');
        }
        
        // Handle continuation
        if is_continuation {
            if !in_continuation {
                // Previous line needs & at end
                if let Some(last_line) = free_format_lines.last_mut() {
                    if !last_line.trim().is_empty() && !last_line.trim().ends_with('&') {
                        last_line.push_str(" &");
                    }
                }
            }
            // Current line starts with &
            free_line.push_str("    & ");
            free_line.push_str(code_part.trim_start());
            in_continuation = true;
        } else {
            // Regular line
            in_continuation = false;
            
            // Add proper indentation based on content
            let trimmed_code = code_part.trim();
            if !trimmed_code.is_empty() {
                let indent = get_free_format_indent(trimmed_code);
                free_line.push_str(&" ".repeat(indent));
                free_line.push_str(trimmed_code);
            }
        }
        
        if !free_line.trim().is_empty() {
            free_format_lines.push(free_line);
        }
    }
    
    Ok(free_format_lines.join("\n"))
}

fn get_free_format_indent(code: &str) -> usize {
    let code_upper = code.to_uppercase();
    
    // No indentation for these constructs
    if code_upper.starts_with("PROGRAM ") ||
       code_upper.starts_with("SUBROUTINE ") ||
       code_upper.starts_with("FUNCTION ") ||
       code_upper.starts_with("MODULE ") ||
       code_upper.starts_with("END") ||
       code_upper.starts_with("CONTAINS") {
        return 0;
    }
    
    // Less indentation for these
    if code_upper.starts_with("IMPLICIT ") ||
       code_upper.starts_with("USE ") ||
       code_upper.starts_with("INTEGER") ||
       code_upper.starts_with("REAL") ||
       code_upper.starts_with("CHARACTER") ||
       code_upper.starts_with("LOGICAL") ||
       code_upper.starts_with("COMPLEX") ||
       code_upper.starts_with("DOUBLE PRECISION") ||
       code_upper.starts_with("PARAMETER") {
        return 4;
    }
    
    // More indentation for executable statements
    if code_upper.starts_with("IF ") ||
       code_upper.starts_with("DO ") ||
       code_upper.starts_with("CALL ") ||
       code_upper.starts_with("PRINT ") ||
       code_upper.starts_with("WRITE ") ||
       code_upper.starts_with("READ ") ||
       code_upper.starts_with("RETURN") ||
       code_upper.starts_with("STOP") ||
       code_upper.contains(" = ") {  // Assignment
        return 4;
    }
    
    // Default indentation
    4
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_subroutine() {
        let fixed_format = r#"
      SUBROUTINE HELLO
      PRINT *, 'Hello World'
      END
"#;
        
        let result = convert_fixed_to_free(fixed_format).unwrap();
        let lines: Vec<&str> = result.lines().collect();
        
        assert!(lines.iter().any(|line| line.contains("subroutine hello") || line.contains("SUBROUTINE HELLO")));
        assert!(lines.iter().any(|line| line.contains("print") || line.contains("PRINT")));
        assert!(lines.iter().any(|line| line.contains("end") || line.contains("END")));
    }

    #[test]
    fn test_comment_conversion() {
        let fixed_format = r#"
c This is a comment
C Another comment
* Yet another comment
      PRINT *, 'Hello'
"#;
        
        let result = convert_fixed_to_free(fixed_format).unwrap();
        
        // Should convert c, C, * comments to ! comments
        assert!(result.contains("! This is a comment"));
        assert!(result.contains("! Another comment"));
        assert!(result.contains("! Yet another comment"));
    }

    #[test]
    fn test_statement_labels() {
        let fixed_format = r#"
10    CONTINUE
20    FORMAT(I5)
"#;
        
        let result = convert_fixed_to_free(fixed_format).unwrap();
        
        assert!(result.contains("10 continue") || result.contains("10 CONTINUE"));
        assert!(result.contains("20 format") || result.contains("20 FORMAT"));
    }
}