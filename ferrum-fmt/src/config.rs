/// Configuration for FORTRAN code formatting.
#[derive(Debug, Clone)]
pub struct FormatConfig {
    /// Number of spaces per indentation level (default: 2)
    pub indent_width: usize,
    
    /// Use spaces instead of tabs (default: true)
    pub use_spaces: bool,
    
    /// Keyword case convention: "upper", "lower", or "preserve" (default: "preserve")
    pub keyword_case: KeywordCase,
    
    /// Identifier case convention: "lower", "upper", or "preserve" (default: "preserve")
    pub identifier_case: IdentifierCase,
    
    /// Spacing around operators (default: true)
    pub space_around_operators: bool,
    
    /// Align declarations in columns (default: true)
    pub align_declarations: bool,
    
    /// Maximum line length before wrapping (default: 132)
    pub max_line_length: usize,
    
    /// Preserve existing line continuations (default: true)
    pub preserve_continuations: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KeywordCase {
    Upper,
    Lower,
    Preserve,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdentifierCase {
    Upper,
    Lower,
    Preserve,
}

impl Default for FormatConfig {
    fn default() -> Self {
        Self {
            indent_width: 2,
            use_spaces: true,
            keyword_case: KeywordCase::Preserve,
            identifier_case: IdentifierCase::Preserve,
            space_around_operators: true,
            align_declarations: true,
            max_line_length: 132,
            preserve_continuations: true,
        }
    }
}

impl FormatConfig {
    /// Create a new config with FORTRAN 77 style (uppercase keywords, 6-space indent).
    pub fn fortran77() -> Self {
        Self {
            indent_width: 6,
            use_spaces: true,
            keyword_case: KeywordCase::Upper,
            identifier_case: IdentifierCase::Preserve,
            space_around_operators: true,
            align_declarations: true,
            max_line_length: 72,
            preserve_continuations: true,
        }
    }
    
    /// Create a new config with modern FORTRAN style (lowercase keywords, 2-space indent).
    pub fn modern() -> Self {
        Self {
            indent_width: 2,
            use_spaces: true,
            keyword_case: KeywordCase::Lower,
            identifier_case: IdentifierCase::Lower,
            space_around_operators: true,
            align_declarations: true,
            max_line_length: 132,
            preserve_continuations: true,
        }
    }
}

