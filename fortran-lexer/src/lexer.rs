use crate::error::{LexError, LexResult};
use crate::token::{Token, TokenType, lookup_keyword};

/// FORTRAN format (fixed or free).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Format {
    FixedFormat,
    FreeFormat,
}

/// Main lexer entry point.
pub fn tokenize(source: &str, format: Format) -> LexResult<Vec<Token>> {
    match format {
        Format::FreeFormat => {
            let mut lexer = FreeFormatLexer::new(source);
            lexer.tokenize()
        }
        Format::FixedFormat => {
            let mut lexer = FixedFormatLexer::new(source);
            lexer.tokenize()
        }
    }
}

/// Detect FORTRAN format from source code.
pub fn detect_format(source: &str) -> Format {
    // Heuristic: check for fixed-format indicators
    let lines: Vec<&str> = source.lines().take(20).collect();
    let total_lines = lines.len();
    let mut fixed_format_count = 0;
    
    for line in &lines {
        if line.is_empty() {
            continue;
        }
        
        // Check for fixed-format comment indicator (c, C, * in column 1)
        if let Some(first_char) = line.chars().next() {
            if first_char == 'c' || first_char == 'C' || first_char == '*' {
                fixed_format_count += 1;
                continue;
            }
        }
        
        // Check for fixed-format structure (columns 1-5 label, column 6 continuation, column 7+ code)
        if line.len() >= 7 {
            let col6 = line.chars().nth(5).unwrap_or(' ');
            let col7 = line.chars().nth(6).unwrap_or(' ');
            
            // In fixed format, column 6 is continuation (0 or space) and column 7+ has code
            if (col6 == '0' || col6 == ' ') && col7 != ' ' && col7 != '\t' {
                // Check if columns 1-5 are numeric or spaces
                let cols_1_5: String = line.chars().take(5).collect();
                if cols_1_5.trim().is_empty() || cols_1_5.chars().all(|c| c.is_ascii_digit() || c == ' ') {
                    fixed_format_count += 1;
                }
            }
        }
    }
    
    // If most lines match fixed format pattern, assume fixed format
    if total_lines > 0 && fixed_format_count > total_lines / 2 {
        Format::FixedFormat
    } else {
        Format::FreeFormat
    }
}

/// Free-format FORTRAN lexer.
pub struct FreeFormatLexer<'a> {
    #[allow(dead_code)] // May be used in future for better error reporting
    source: &'a str,
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    current_line: usize,
    current_column: usize,
    position: usize,
    tokens: Vec<Token>,
}

impl<'a> FreeFormatLexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars().peekable(),
            current_line: 1,
            current_column: 1,
            position: 0,
            tokens: Vec::new(),
        }
    }
    
    pub fn tokenize(&mut self) -> LexResult<Vec<Token>> {
        loop {
            if self.is_at_end() {
                break;
            }
            self.skip_whitespace();
            if self.is_at_end() {
                break;
            }
            
            let token = self.next_token()?;
            self.tokens.push(token);
        }
        
        // Add EOF token
        self.tokens.push(Token::new(
            TokenType::Eof,
            String::new(),
            self.current_line,
            self.current_column,
            self.position,
            self.position,
        ));
        
        Ok(self.tokens.clone())
    }
    
    fn next_token(&mut self) -> LexResult<Token> {
        let start_pos = self.position;
        let start_line = self.current_line;
        let start_col = self.current_column;
        
        let ch = self.advance().ok_or(LexError::UnexpectedChar {
            ch: '\0',
            line: start_line,
            column: start_col,
        })?;
        
        match ch {
            // Comments
            '!' => self.tokenize_comment(start_pos, start_line, start_col),
            
            // String literals
            '"' | '\'' => self.tokenize_string(ch, start_pos, start_line, start_col),
            
            // Numbers
            '0'..='9' => self.tokenize_number(ch, start_pos, start_line, start_col),
            
            // Operators and punctuation
            '+' => Ok(self.create_token(TokenType::Plus, "+", start_pos, start_line, start_col)),
            '-' => Ok(self.create_token(TokenType::Minus, "-", start_pos, start_line, start_col)),
            '*' => {
                if self.matches('*') {
                    self.advance();
                    Ok(self.create_token(TokenType::Power, "**", start_pos, start_line, start_col))
                } else {
                    Ok(self.create_token(TokenType::Multiply, "*", start_pos, start_line, start_col))
                }
            }
            '/' => {
                if self.matches('=') {
                    self.advance();
                    Ok(self.create_token(TokenType::NotEquals, "/=", start_pos, start_line, start_col))
                } else {
                    Ok(self.create_token(TokenType::Divide, "/", start_pos, start_line, start_col))
                }
            }
            '=' => {
                if self.matches('>') {
                    self.advance();
                    Ok(self.create_token(TokenType::Assignment, "=>", start_pos, start_line, start_col))
                } else {
                    Ok(self.create_token(TokenType::Equals, "=", start_pos, start_line, start_col))
                }
            }
            '<' => {
                if self.matches('=') {
                    self.advance();
                    Ok(self.create_token(TokenType::LessOrEqual, "<=", start_pos, start_line, start_col))
                } else {
                    Ok(self.create_token(TokenType::LessThan, "<", start_pos, start_line, start_col))
                }
            }
            '>' => {
                if self.matches('=') {
                    self.advance();
                    Ok(self.create_token(TokenType::GreaterOrEqual, ">=", start_pos, start_line, start_col))
                } else {
                    Ok(self.create_token(TokenType::GreaterThan, ">", start_pos, start_line, start_col))
                }
            }
            '(' => Ok(self.create_token(TokenType::LeftParen, "(", start_pos, start_line, start_col)),
            ')' => Ok(self.create_token(TokenType::RightParen, ")", start_pos, start_line, start_col)),
            ',' => Ok(self.create_token(TokenType::Comma, ",", start_pos, start_line, start_col)),
            ';' => Ok(self.create_token(TokenType::Semicolon, ";", start_pos, start_line, start_col)),
            ':' => {
                if self.matches(':') {
                    self.advance();
                    Ok(self.create_token(TokenType::DoubleColon, "::", start_pos, start_line, start_col))
                } else {
                    Ok(self.create_token(TokenType::Colon, ":", start_pos, start_line, start_col))
                }
            }
            '.' => self.tokenize_period(start_pos, start_line, start_col),
            
            // Identifiers and keywords
            'a'..='z' | 'A'..='Z' | '_' => self.tokenize_identifier(ch, start_pos, start_line, start_col),
            
            _ => Err(LexError::UnexpectedChar {
                ch,
                line: start_line,
                column: start_col,
            }),
        }
    }
    
    fn tokenize_comment(&mut self, start_pos: usize, start_line: usize, start_col: usize) -> LexResult<Token> {
        let mut comment = String::new();
        while let Some(&ch) = self.chars.peek() {
            if ch == '\n' {
                break;
            }
            comment.push(self.advance().unwrap());
        }
        let lexeme = format!("!{}", comment);
        Ok(self.create_token(TokenType::Comment(comment), &lexeme, start_pos, start_line, start_col))
    }
    
    fn tokenize_string(&mut self, quote: char, start_pos: usize, start_line: usize, start_col: usize) -> LexResult<Token> {
        let mut value = String::new();
        let mut escaped = false;
        
        while let Some(&ch) = self.chars.peek() {
            if escaped {
                value.push(ch);
                self.advance();
                escaped = false;
            } else if ch == '\\' {
                escaped = true;
                self.advance();
            } else if ch == quote {
                self.advance(); // consume closing quote
                let lexeme = format!("{}{}{}", quote, value, quote);
                return Ok(self.create_token(
                    TokenType::CharacterLiteral(value),
                    &lexeme,
                    start_pos,
                    start_line,
                    start_col,
                ));
            } else {
                value.push(ch);
                self.advance();
            }
        }
        
        Err(LexError::UnterminatedString {
            line: start_line,
            column: start_col,
        })
    }
    
    fn tokenize_number(&mut self, first_digit: char, start_pos: usize, start_line: usize, start_col: usize) -> LexResult<Token> {
        let mut value = String::from(first_digit);
        let mut has_dot = false;
        let mut has_exponent = false;
        
        while let Some(&ch) = self.chars.peek() {
            match ch {
                '0'..='9' => {
                    value.push(self.advance().unwrap());
                }
                '.' if !has_dot && !has_exponent => {
                    has_dot = true;
                    value.push(self.advance().unwrap());
                }
                'e' | 'E' | 'd' | 'D' if !has_exponent => {
                    has_exponent = true;
                    value.push(self.advance().unwrap());
                    // Optional sign after exponent
                    if let Some(&sign) = self.chars.peek() {
                        if sign == '+' || sign == '-' {
                            value.push(self.advance().unwrap());
                        }
                    }
                }
                '_' => {
                    // FORTRAN allows underscores in numeric literals for kind parameter
                    self.advance();
                    if let Some(&ch) = self.chars.peek() {
                        if ch.is_ascii_alphanumeric() {
                            value.push('_');
                            value.push(self.advance().unwrap());
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        
        let token_type = if has_dot || has_exponent {
            TokenType::RealLiteral(value.clone())
        } else {
            TokenType::IntegerLiteral(value.clone())
        };
        
        Ok(self.create_token(token_type, &value, start_pos, start_line, start_col))
    }
    
    fn tokenize_period(&mut self, start_pos: usize, start_line: usize, start_col: usize) -> LexResult<Token> {
        // Check if it's a logical operator or literal (.AND., .TRUE., etc.)
        let mut value = String::from('.');
        
        // Collect characters until we find a closing period or non-alphabetic
        while let Some(&ch) = self.chars.peek() {
            if ch.is_ascii_alphabetic() {
                value.push(self.advance().unwrap());
            } else if ch == '.' && value.len() > 1 {
                // Found closing period, check what it is
                value.push(self.advance().unwrap());
                let upper = value.to_uppercase();
                
                // Handle logical literals first
                if upper == ".TRUE." {
                    return Ok(self.create_token(
                        TokenType::LogicalLiteral(String::from("true")),
                        &value,
                        start_pos,
                        start_line,
                        start_col,
                    ));
                } else if upper == ".FALSE." {
                    return Ok(self.create_token(
                        TokenType::LogicalLiteral(String::from("false")),
                        &value,
                        start_pos,
                        start_line,
                        start_col,
                    ));
                }
                
                // Check if it's a logical operator keyword
                if let Some(token_type) = lookup_keyword(&value) {
                    return Ok(self.create_token(token_type, &value, start_pos, start_line, start_col));
                }
                
                // Not a recognized keyword, break and treat as period
                break;
            } else {
                // Not a logical operator/literal pattern
                break;
            }
        }
        
        // Just a period (or unrecognized pattern starting with period)
        // If we collected more than just ".", we need to handle it
        if value.len() == 1 {
            Ok(self.create_token(TokenType::Period, ".", start_pos, start_line, start_col))
        } else {
            // We have something like ".ABC" - treat as period followed by identifier
            // This is an error case, but we'll return the period
            // The remaining characters will be tokenized as an identifier in the next call
            Ok(self.create_token(TokenType::Period, ".", start_pos, start_line, start_col))
        }
    }
    
    fn tokenize_identifier(&mut self, first_char: char, start_pos: usize, start_line: usize, start_col: usize) -> LexResult<Token> {
        let mut value = String::from(first_char);
        
        while let Some(&ch) = self.chars.peek() {
            if ch.is_ascii_alphanumeric() || ch == '_' {
                value.push(self.advance().unwrap());
            } else {
                break;
            }
        }
        
        // Check if it's a keyword
        let token_type = lookup_keyword(&value)
            .unwrap_or_else(|| TokenType::Identifier(value.clone()));
        
        Ok(self.create_token(token_type, &value, start_pos, start_line, start_col))
    }
    
    fn skip_whitespace(&mut self) {
        let mut whitespace = String::new();
        let start_pos = self.position;
        let start_line = self.current_line;
        let start_col = self.current_column;
        
        while let Some(&ch) = self.chars.peek() {
            match ch {
                ' ' | '\t' | '\r' => {
                    whitespace.push(self.advance().unwrap());
                }
                '\n' => {
                    whitespace.push(self.advance().unwrap());
                    self.current_line += 1;
                    self.current_column = 1;
                }
                _ => break,
            }
        }
        
        // Optionally preserve whitespace tokens
        if !whitespace.is_empty() {
            let whitespace_clone = whitespace.clone();
            self.tokens.push(Token::new(
                TokenType::Whitespace(whitespace),
                whitespace_clone,
                start_line,
                start_col,
                start_pos,
                self.position,
            ));
        }
    }
    
    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.next()?;
        self.position += ch.len_utf8();
        if ch != '\n' {
            self.current_column += 1;
        }
        Some(ch)
    }
    
    fn matches(&mut self, expected: char) -> bool {
        self.chars.peek().is_some_and(|&ch| ch == expected)
    }
    
    fn is_at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }
    
    fn create_token(&self, token_type: TokenType, lexeme: &str, start_pos: usize, line: usize, column: usize) -> Token {
        let end_pos = start_pos + lexeme.len();
        Token::new(token_type, lexeme.to_string(), line, column, start_pos, end_pos)
    }
}

/// Fixed-format FORTRAN lexer (FORTRAN 77 and earlier).
/// 
/// Handles the column-based format where:
/// - Columns 1-5: Statement labels (optional)
/// - Column 6: Continuation indicator (space/0 = new statement, other = continuation)
/// - Columns 7-72: FORTRAN code
/// - Columns 73-80: Comments/sequence numbers (ignored)
/// - Column 1 = C, c, *, !: Comment line
pub struct FixedFormatLexer<'a> {
    source: &'a str,
    lines: Vec<&'a str>,
    current_line_index: usize,
    tokens: Vec<Token>,
}

impl<'a> FixedFormatLexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let lines: Vec<&str> = source.lines().collect();
        Self {
            source,
            lines,
            current_line_index: 0,
            tokens: Vec::new(),
        }
    }
    
    pub fn tokenize(&mut self) -> LexResult<Vec<Token>> {
        // Process each line according to fixed-format rules
        while self.current_line_index < self.lines.len() {
            self.process_line()?;
            self.current_line_index += 1;
        }
        
        // Add EOF token
        let eof_token = Token::new(
            TokenType::Eof,
            String::new(),
            self.current_line_index + 1,
            1,
            self.source.len(),
            self.source.len()
        );
        self.tokens.push(eof_token);
        
        Ok(self.tokens.clone())
    }
    
    fn process_line(&mut self) -> LexResult<()> {
        let line = self.lines[self.current_line_index];
        let line_number = self.current_line_index + 1;
        
        // Handle empty lines
        if line.trim().is_empty() {
            return Ok(());
        }
        
        // Check for comment lines (C, c, *, ! in column 1)
        if let Some(first_char) = line.chars().next() {
            if matches!(first_char, 'C' | 'c' | '*' | '!') {
                self.tokenize_comment_line(line, line_number)?;
                return Ok(());
            }
        }
        
        // Process fixed-format statement line
        self.process_statement_line(line, line_number)
    }
    
    fn tokenize_comment_line(&mut self, line: &str, line_number: usize) -> LexResult<()> {
        let start_pos = self.calculate_byte_position(line_number, 1);
        let comment_token = Token::new(
            TokenType::Comment(line.to_string()),
            line.to_string(),
            line_number,
            1,
            start_pos,
            start_pos + line.len()
        );
        self.tokens.push(comment_token);
        Ok(())
    }
    
    fn process_statement_line(&mut self, line: &str, line_number: usize) -> LexResult<()> {
        // FORTRAN 77 fixed format:
        // Cols 1-5: Statement label (optional)
        // Col 6: Continuation indicator
        // Cols 7-72: FORTRAN code
        // Cols 73-80: Comments (ignored)
        
        let chars: Vec<char> = line.chars().collect();
        
        // Handle short lines
        if chars.is_empty() {
            return Ok(());
        }
        
        // Extract statement label (columns 1-5)
        let label_part = if chars.len() >= 5 {
            chars[0..5].iter().collect::<String>()
        } else {
            chars.iter().collect::<String>()
        };
        
        // Check for statement label
        let label_trimmed = label_part.trim();
        if !label_trimmed.is_empty() && label_trimmed.chars().all(|c| c.is_ascii_digit()) {
            let start_pos = self.calculate_byte_position(line_number, 1);
            let label_token = Token::new(
                TokenType::IntegerLiteral(label_trimmed.to_string()),
                label_trimmed.to_string(),
                line_number,
                1,
                start_pos,
                start_pos + label_trimmed.len()
            );
            self.tokens.push(label_token);
        }
        
        // Check continuation indicator (column 6)
        let is_continuation = if chars.len() >= 6 {
            let continuation_char = chars[5];
            continuation_char != ' ' && continuation_char != '0'
        } else {
            false
        };
        
        // Extract code part (columns 7-72)
        let code_start = 6; // 0-indexed, so column 7
        let code_end = if chars.len() > 72 { 72 } else { chars.len() };
        
        if code_start < chars.len() {
            let code_part: String = chars[code_start..code_end].iter().collect();
            
            // Tokenize the code part using a similar approach to free-format
            // but adapted for the fixed-format context
            self.tokenize_code_segment(&code_part, line_number, code_start + 1, is_continuation)?;
        }
        
        Ok(())
    }
    
    fn tokenize_code_segment(&mut self, code: &str, line_number: usize, start_column: usize, _is_continuation: bool) -> LexResult<()> {
        // Create a mini free-format lexer for the code segment
        let mut segment_lexer = FreeFormatLexer::new(code);
        let segment_tokens = segment_lexer.tokenize()?;
        
        // Adjust the tokens to have correct line/column positions
        for token in segment_tokens {
            if token.token_type == TokenType::Eof {
                continue; // Skip EOF tokens from segments
            }
            
            let lexeme_len = token.lexeme.len();
            let adjusted_token = Token::new(
                token.token_type,
                token.lexeme,
                line_number,
                start_column + token.column - 1,
                self.calculate_byte_position(line_number, start_column + token.column - 1),
                self.calculate_byte_position(line_number, start_column + token.column - 1) + lexeme_len
            );
            self.tokens.push(adjusted_token);
        }
        
        Ok(())
    }
    
    fn calculate_byte_position(&self, line_number: usize, column: usize) -> usize {
        let mut pos = 0;
        
        // Add bytes from previous lines
        for i in 0..(line_number - 1) {
            if i < self.lines.len() {
                pos += self.lines[i].len() + 1; // +1 for newline
            }
        }
        
        // Add bytes from current line up to column
        if line_number > 0 && line_number <= self.lines.len() {
            let line = self.lines[line_number - 1];
            let chars: Vec<char> = line.chars().collect();
            for i in 0..(column - 1).min(chars.len()) {
                pos += chars[i].len_utf8();
            }
        }
        
        pos
    }
}

