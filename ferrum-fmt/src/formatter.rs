use fortran_lexer::{Token, TokenType};
use fortran_ast::Program;
use crate::config::{FormatConfig, KeywordCase, IdentifierCase};

/// Formatter for FORTRAN source code.
pub struct Formatter {
    config: FormatConfig,
}

impl Formatter {
    pub fn new(config: FormatConfig) -> Self {
        Self { config }
    }
    
    /// Format FORTRAN source code from tokens.
    /// Uses AST for indentation depth calculation.
    pub fn format(&self, tokens: &[Token], ast: Option<&Program>) -> String {
        let mut output = String::new();
        let mut current_line = String::new();
        let mut current_indent = 0;
        
        // Track indentation depth from AST if available
        let indent_map = if let Some(prog) = ast {
            self.compute_indentation(prog, tokens)
        } else {
            self.compute_indentation_from_tokens(tokens)
        };
        
        let mut i = 0;
        while i < tokens.len() {
            let token = &tokens[i];
            
            // Skip EOF
            if matches!(token.token_type, TokenType::Eof) {
                break;
            }
            
            // Handle comments - preserve as-is
            if let TokenType::Comment(ref comment) = token.token_type {
                if !current_line.trim().is_empty() {
                    output.push_str(&current_line);
                    output.push('\n');
                    current_line.clear();
                }
                output.push_str(&comment);
                output.push('\n');
                i += 1;
                continue;
            }
            
            // Skip whitespace - we'll add our own
            if matches!(token.token_type, TokenType::Whitespace(_)) {
                i += 1;
                continue;
            }
            
            // Get indentation for this token
            let token_indent = indent_map.get(&i).copied().unwrap_or(0);
            
            // Start new line if needed
            if current_line.trim().is_empty() {
                current_indent = token_indent;
                current_line = self.indent_string(current_indent);
            }
            
            // Format the token
            let formatted = self.format_token(token);
            
            // Check if we need to wrap the line
            let line_length = current_line.len();
            if line_length + formatted.len() > self.config.max_line_length && !current_line.trim().is_empty() {
                output.push_str(&current_line.trim_end());
                output.push('&');
                output.push('\n');
                current_line = self.indent_string(current_indent + 1);
            }
            
            // Add spacing before token if needed
            if !current_line.trim().is_empty() {
                if self.needs_space_before(tokens.get(i.saturating_sub(1)), token) {
                    current_line.push(' ');
                }
            }
            
            current_line.push_str(&formatted);
            
            // Check if this token ends a statement
            if self.is_statement_terminator(token) {
                output.push_str(&current_line);
                output.push('\n');
                current_line.clear();
            }
            
            i += 1;
        }
        
        // Add remaining line
        if !current_line.trim().is_empty() {
            output.push_str(&current_line);
            output.push('\n');
        }
        
        output
    }
    
    fn format_token(&self, token: &Token) -> String {
        match &token.token_type {
            TokenType::Identifier(ref name) => {
                match self.config.identifier_case {
                    IdentifierCase::Upper => name.to_uppercase(),
                    IdentifierCase::Lower => name.to_lowercase(),
                    IdentifierCase::Preserve => name.clone(),
                }
            }
            keyword_type if self.is_keyword(keyword_type) => {
                let keyword = self.keyword_to_string(keyword_type);
                match self.config.keyword_case {
                    KeywordCase::Upper => keyword.to_uppercase(),
                    KeywordCase::Lower => keyword.to_lowercase(),
                    KeywordCase::Preserve => token.lexeme.clone(),
                }
            }
            TokenType::Plus => {
                if self.config.space_around_operators { "+".to_string() } else { "+".to_string() }
            }
            TokenType::Minus => {
                if self.config.space_around_operators { "-".to_string() } else { "-".to_string() }
            }
            TokenType::Multiply => "*".to_string(),
            TokenType::Divide => "/".to_string(),
            TokenType::Power => "**".to_string(),
            TokenType::Equals => "=".to_string(),
            TokenType::NotEquals => "/=".to_string(),
            TokenType::LessThan => "<".to_string(),
            TokenType::LessOrEqual => "<=".to_string(),
            TokenType::GreaterThan => ">".to_string(),
            TokenType::GreaterOrEqual => ">=".to_string(),
            TokenType::And => ".and.".to_string(),
            TokenType::Or => ".or.".to_string(),
            TokenType::Not => ".not.".to_string(),
            TokenType::LeftParen => "(".to_string(),
            TokenType::RightParen => ")".to_string(),
            TokenType::Comma => ",".to_string(),
            TokenType::Semicolon => ";".to_string(),
            TokenType::Colon => ":".to_string(),
            TokenType::Period => ".".to_string(),
            TokenType::DoubleColon => "::".to_string(),
            TokenType::Assignment => "=>".to_string(),
            TokenType::IntegerLiteral(ref s) | 
            TokenType::RealLiteral(ref s) | 
            TokenType::ComplexLiteral(ref s) |
            TokenType::CharacterLiteral(ref s) |
            TokenType::LogicalLiteral(ref s) => s.clone(),
            _ => token.lexeme.clone(),
        }
    }
    
    fn is_keyword(&self, token_type: &TokenType) -> bool {
        matches!(token_type,
            TokenType::Program | TokenType::Subroutine | TokenType::Function |
            TokenType::Module | TokenType::EndProgram | TokenType::EndSubroutine |
            TokenType::EndFunction | TokenType::EndModule | TokenType::Use |
            TokenType::Contains | TokenType::Integer | TokenType::Real |
            TokenType::DoublePrecision | TokenType::Complex | TokenType::Character |
            TokenType::Logical | TokenType::Parameter | TokenType::Allocatable |
            TokenType::Dimension | TokenType::Intent | TokenType::If | TokenType::Then |
            TokenType::Else | TokenType::ElseIf | TokenType::EndIf | TokenType::Do |
            TokenType::EndDo | TokenType::DoWhile | TokenType::Select | TokenType::Case |
            TokenType::EndSelect | TokenType::Default | TokenType::Continue |
            TokenType::Cycle | TokenType::Exit | TokenType::Stop | TokenType::Return |
            TokenType::Implicit | TokenType::None | TokenType::NoneExplicit |
            TokenType::Public | TokenType::Private | TokenType::Save | TokenType::Data |
            TokenType::Write | TokenType::Read | TokenType::Print | TokenType::Open |
            TokenType::Close | TokenType::Format | TokenType::Inquire | TokenType::Backspace |
            TokenType::Rewind | TokenType::Endfile
        )
    }
    
    fn keyword_to_string(&self, token_type: &TokenType) -> &str {
        match token_type {
            TokenType::Program => "program",
            TokenType::Subroutine => "subroutine",
            TokenType::Function => "function",
            TokenType::Module => "module",
            TokenType::EndProgram => "end program",
            TokenType::EndSubroutine => "end subroutine",
            TokenType::EndFunction => "end function",
            TokenType::EndModule => "end module",
            TokenType::Use => "use",
            TokenType::Contains => "contains",
            TokenType::Integer => "integer",
            TokenType::Real => "real",
            TokenType::DoublePrecision => "double precision",
            TokenType::Complex => "complex",
            TokenType::Character => "character",
            TokenType::Logical => "logical",
            TokenType::Parameter => "parameter",
            TokenType::Allocatable => "allocatable",
            TokenType::Dimension => "dimension",
            TokenType::Intent => "intent",
            TokenType::If => "if",
            TokenType::Then => "then",
            TokenType::Else => "else",
            TokenType::ElseIf => "elseif",
            TokenType::EndIf => "end if",
            TokenType::Do => "do",
            TokenType::EndDo => "end do",
            TokenType::DoWhile => "do while",
            TokenType::Select => "select",
            TokenType::Case => "case",
            TokenType::EndSelect => "end select",
            TokenType::Default => "default",
            TokenType::Continue => "continue",
            TokenType::Cycle => "cycle",
            TokenType::Exit => "exit",
            TokenType::Stop => "stop",
            TokenType::Return => "return",
            TokenType::Implicit => "implicit",
            TokenType::None => "none",
            TokenType::NoneExplicit => "none",
            TokenType::Public => "public",
            TokenType::Private => "private",
            TokenType::Save => "save",
            TokenType::Data => "data",
            TokenType::Write => "write",
            TokenType::Read => "read",
            TokenType::Print => "print",
            TokenType::Open => "open",
            TokenType::Close => "close",
            TokenType::Format => "format",
            TokenType::Inquire => "inquire",
            TokenType::Backspace => "backspace",
            TokenType::Rewind => "rewind",
            TokenType::Endfile => "endfile",
            _ => "",
        }
    }
    
    fn needs_space_before(&self, prev: Option<&Token>, current: &Token) -> bool {
        if prev.is_none() {
            return false;
        }
        let prev_type = &prev.unwrap().token_type;
        
        // No space after opening paren, before closing paren
        if matches!(prev_type, TokenType::LeftParen) || matches!(current.token_type, TokenType::RightParen) {
            return false;
        }
        
        // No space before comma, semicolon, period
        if matches!(current.token_type, TokenType::Comma | TokenType::Semicolon | TokenType::Period) {
            return false;
        }
        
        // Space after comma, semicolon, colon (except ::)
        if matches!(prev_type, TokenType::Comma | TokenType::Semicolon | TokenType::Colon) {
            return !matches!(current.token_type, TokenType::DoubleColon);
        }
        
        // Space around operators if configured
        if self.config.space_around_operators {
            if self.is_operator(prev_type) || self.is_operator(&current.token_type) {
                return true;
            }
        }
        
        // Default: space between different token types
        true
    }
    
    fn is_operator(&self, token_type: &TokenType) -> bool {
        matches!(token_type,
            TokenType::Plus | TokenType::Minus | TokenType::Multiply |
            TokenType::Divide | TokenType::Power | TokenType::Equals |
            TokenType::NotEquals | TokenType::LessThan | TokenType::LessOrEqual |
            TokenType::GreaterThan | TokenType::GreaterOrEqual | TokenType::And |
            TokenType::Or | TokenType::Not | TokenType::Eqv | TokenType::Neqv
        )
    }
    
    fn is_statement_terminator(&self, token: &Token) -> bool {
        // In FORTRAN, statements end at newlines (or semicolons)
        matches!(token.token_type, TokenType::Semicolon)
    }
    
    fn indent_string(&self, level: usize) -> String {
        if self.config.use_spaces {
            " ".repeat(level * self.config.indent_width)
        } else {
            "\t".repeat(level)
        }
    }
    
    fn compute_indentation(&self, _program: &Program, _tokens: &[Token]) -> std::collections::HashMap<usize, usize> {
        // TODO: Use AST to compute accurate indentation
        // For now, fall back to token-based heuristics
        self.compute_indentation_from_tokens(_tokens)
    }
    
    fn compute_indentation_from_tokens(&self, tokens: &[Token]) -> std::collections::HashMap<usize, usize> {
        let mut indent_map = std::collections::HashMap::new();
        let mut indent_level: usize = 0;
        
        for (i, token) in tokens.iter().enumerate() {
            // Increase indent after certain keywords
            if matches!(token.token_type,
                TokenType::If | TokenType::Do | TokenType::DoWhile |
                TokenType::Select | TokenType::Function | TokenType::Subroutine |
                TokenType::Module | TokenType::Program | TokenType::Case
            ) {
                indent_map.insert(i, indent_level);
                indent_level += 1;
            } else if matches!(token.token_type,
                TokenType::EndIf | TokenType::EndDo | TokenType::EndSelect |
                TokenType::EndFunction | TokenType::EndSubroutine |
                TokenType::EndModule | TokenType::EndProgram
            ) {
                indent_level = indent_level.saturating_sub(1);
                indent_map.insert(i, indent_level);
            } else if matches!(token.token_type, TokenType::Then | TokenType::Else | TokenType::ElseIf) {
                indent_map.insert(i, indent_level.saturating_sub(1));
            } else {
                indent_map.insert(i, indent_level);
            }
        }
        
        indent_map
    }
}

