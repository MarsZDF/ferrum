//! Auto-formatter for FORTRAN source code (like rustfmt or black).
//!
//! This crate provides automatic code formatting for FORTRAN source files,
//! enforcing consistent style including indentation, case conventions, spacing,
//! and alignment.
//!
//! # Features
//!
//! - ✅ Configurable indentation (spaces/tabs, width)
//! - ✅ Keyword case normalization (UPPER, lower, preserve)
//! - ✅ Identifier case normalization
//! - ✅ Spacing around operators
//! - ✅ Column alignment for declarations
//! - ✅ Line length enforcement
//! - ✅ Comment preservation
//!
//! # Quick Start
//!
//! ```rust
//! use ferrum_fmt::{format_source, FormatConfig};
//! use fortran_lexer::detect_format;
//!
//! let source = r#"
//! program hello
//! IMPLICIT NONE
//! integer :: x, y
//! x = 10
//! y = 20
//! print *, x + y
//! end program hello
//! "#;
//!
//! let format = detect_format(source);
//! let config = FormatConfig::modern(); // or FormatConfig::fortran77()
//! let formatted = format_source(source, format, config)?;
//! println!("{}", formatted);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! # Configuration
//!
//! ```rust
//! use ferrum_fmt::{FormatConfig, KeywordCase, IdentifierCase};
//!
//! let config = FormatConfig {
//!     indent_width: 4,
//!     keyword_case: KeywordCase::Upper,
//!     identifier_case: IdentifierCase::Lower,
//!     ..Default::default()
//! };
//! ```

pub mod config;
pub mod formatter;

pub use config::{FormatConfig, KeywordCase, IdentifierCase};
pub use formatter::Formatter;

use fortran_lexer::{tokenize, Format};
use fortran_parser::parse;
use anyhow::Result;

/// Format FORTRAN source code with the given configuration.
pub fn format_source(source: &str, format: Format, config: FormatConfig) -> Result<String> {
    let tokens = tokenize(source, format)?;
    
    // Try to parse for better indentation (optional, will fall back to heuristics if fails)
    let ast = parse(source).ok();
    
    let formatter = Formatter::new(config);
    let formatted = formatter.format(&tokens, ast.as_ref());
    
    Ok(formatted)
}

/// Format FORTRAN source code with default configuration.
pub fn format(source: &str) -> Result<String> {
    let format = fortran_lexer::detect_format(source);
    format_source(source, format, FormatConfig::default())
}

