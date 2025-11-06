#![allow(clippy::while_let_loop, clippy::if_same_then_else)] // Allow some clippy warnings for now

use fortran_ast::{
    Declaration, Expression, Intent, Literal, MainProgram, Module, Program, ProgramUnit,
    Span, Spanned, Statement, Subroutine, Function, TypeSpec, Attribute, BinaryOp, UnaryOp,
};
use fortran_ast::program::{Argument, ContainsSection, InternalProcedure};
use fortran_ast::statement::{ElseIfClause, CaseClause, CaseSelector};
use fortran_lexer::{tokenize, detect_format, Token, TokenType};
use crate::error::{ParseError, ParseResult};

/// FORTRAN parser.
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    /// Create a new parser from source code.
    pub fn new(source: &str) -> ParseResult<Self> {
        let format = detect_format(source);
        let tokens = tokenize(source, format)?;
        Ok(Self {
            tokens,
            current: 0,
        })
    }
    
    /// Parse a complete FORTRAN program.
    pub fn parse_program(&mut self) -> ParseResult<Program> {
        let program_unit = self.parse_program_unit()?;
        
        Ok(Program {
            name: None,
            program_unit,
        })
    }
    
    /// Parse a program unit (PROGRAM, SUBROUTINE, FUNCTION, or MODULE).
    fn parse_program_unit(&mut self) -> ParseResult<ProgramUnit> {
        if self.check_token(&TokenType::Program) {
            self.parse_main_program()
                .map(ProgramUnit::MainProgram)
        } else if self.check_token(&TokenType::Subroutine) {
            self.parse_subroutine()
                .map(ProgramUnit::Subroutine)
        } else if self.check_token(&TokenType::Function) {
            self.parse_function()
                .map(ProgramUnit::Function)
        } else if self.check_token(&TokenType::Module) {
            self.parse_module()
                .map(ProgramUnit::Module)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: vec!["PROGRAM, SUBROUTINE, FUNCTION, or MODULE".to_string()],
                found: self.peek().cloned().unwrap_or_else(|| self.create_eof_token()),
            })
        }
    }
    
    /// Parse a main program.
    fn parse_main_program(&mut self) -> ParseResult<MainProgram> {
        let _start = self.consume_token(&TokenType::Program)?;
        let name = self.parse_identifier_opt();
        
        let declarations = self.parse_declarations()?;
        let executable_statements = self.parse_executable_statements()?;
        
        // Optional CONTAINS section
        let contains = if self.check_token(&TokenType::Contains) {
            Some(self.parse_contains()?)
        } else {
            None
        };
        
        // END PROGRAM
        if self.check_token(&TokenType::EndProgram) {
            self.advance();
            if let Some(_program_name) = self.parse_identifier_opt() {
                // Name optional but can be present
            }
        }
        
        Ok(MainProgram {
            name,
            declarations,
            executable_statements,
            contains,
        })
    }
    
    /// Parse a subroutine.
    fn parse_subroutine(&mut self) -> ParseResult<Subroutine> {
        let _start = self.consume_token(&TokenType::Subroutine)?;
        let name = self.parse_identifier()
            .ok_or_else(|| ParseError::UnexpectedToken {
                expected: vec!["identifier".to_string()],
                found: self.peek().cloned().unwrap_or_else(|| self.create_eof_token()),
            })?;
        
        // Parse arguments
        let arguments = if self.check_token(&TokenType::LeftParen) {
            self.advance();
            let args = self.parse_argument_list()?;
            self.consume_token(&TokenType::RightParen)?;
            args
        } else {
            Vec::new()
        };
        
        let declarations = self.parse_declarations()?;
        let executable_statements = self.parse_executable_statements()?;
        
        let contains = if self.check_token(&TokenType::Contains) {
            Some(self.parse_contains()?)
        } else {
            None
        };
        
        // END SUBROUTINE
        if self.check_token(&TokenType::EndSubroutine) {
            self.advance();
        }
        
        Ok(Subroutine {
            name,
            arguments,
            declarations,
            executable_statements,
            contains,
        })
    }
    
    /// Parse a function.
    fn parse_function(&mut self) -> ParseResult<Function> {
        // Optional type specification
        let type_spec = self.parse_type_spec_opt();
        
        let _start = self.consume_token(&TokenType::Function)?;
        let name = self.parse_identifier()
            .ok_or_else(|| ParseError::UnexpectedToken {
                expected: vec!["identifier".to_string()],
                found: self.peek().cloned().unwrap_or_else(|| self.create_eof_token()),
            })?;
        
        // Parse arguments
        let arguments = if self.check_token(&TokenType::LeftParen) {
            self.advance();
            let args = self.parse_argument_list()?;
            self.consume_token(&TokenType::RightParen)?;
            args
        } else {
            Vec::new()
        };
        
        // Optional RESULT clause: result(name)
        let result_name = if let Some(token) = self.peek() {
            if let TokenType::Identifier(ref name) = token.token_type {
                if name.to_uppercase() == "RESULT" {
                    self.advance(); // consume RESULT
                    if self.check_token(&TokenType::LeftParen) {
                        self.advance(); // consume (
                        let name = self.parse_identifier()
                            .ok_or_else(|| ParseError::UnexpectedToken {
                                expected: vec!["identifier".to_string()],
                                found: self.peek().cloned().unwrap_or_else(|| self.create_eof_token()),
                            })?;
                        self.consume_token(&TokenType::RightParen)?;
                        Some(name)
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };
        
        let declarations = self.parse_declarations()?;
        let executable_statements = self.parse_executable_statements()?;
        
        let contains = if self.check_token(&TokenType::Contains) {
            Some(self.parse_contains()?)
        } else {
            None
        };
        
        // END FUNCTION
        if self.check_token(&TokenType::EndFunction) {
            self.advance();
        }
        
        Ok(Function {
            name,
            arguments,
            result_name,
            type_spec,
            declarations,
            executable_statements,
            contains,
        })
    }
    
    /// Parse a module.
    fn parse_module(&mut self) -> ParseResult<Module> {
        let _start = self.consume_token(&TokenType::Module)?;
        let name = self.parse_identifier()
            .ok_or_else(|| ParseError::UnexpectedToken {
                expected: vec!["identifier".to_string()],
                found: self.peek().cloned().unwrap_or_else(|| self.create_eof_token()),
            })?;
        
        let declarations = self.parse_declarations()?;
        
        let contains = if self.check_token(&TokenType::Contains) {
            Some(self.parse_contains()?)
        } else {
            None
        };
        
        // END MODULE
        if self.check_token(&TokenType::EndModule) {
            self.advance();
        }
        
        Ok(Module {
            name,
            declarations,
            contains,
        })
    }
    
    /// Parse declarations.
    fn parse_declarations(&mut self) -> ParseResult<Vec<Spanned<Declaration>>> {
        let mut declarations = Vec::new();
        
        while !self.is_at_end() {
            // Check if we've reached executable statements
            if self.is_executable_statement() {
                break;
            }
            
            // Skip IMPLICIT NONE for now (handle it specially)
            if self.check_token(&TokenType::Implicit) {
                self.advance();
                if self.check_token(&TokenType::None) {
                    self.advance();
                    // Create IMPLICIT NONE declaration
                    let span = self.create_span(0, 0, 1, 1);
                    declarations.push(Spanned::new(
                        Declaration::Implicit {
                            rules: Vec::new(),
                        },
                        span,
                    ));
                    continue;
                }
            }
            
            // Parse type declarations
            if let Some(type_spec) = self.parse_type_spec_opt() {
                // Parse attributes (like intent(in), dimension, etc.) before ::
                // Attributes can be comma-separated: integer, intent(in), dimension(10) :: x
                let mut type_attributes = Vec::new();
                while self.check_token(&TokenType::Comma) {
                    self.advance(); // consume comma
                    // Parse attribute after comma
                    if self.check_token(&TokenType::Intent) {
                        self.advance();
                        self.consume_token(&TokenType::LeftParen)?;
                        let intent_str = self.parse_identifier()
                            .ok_or_else(|| ParseError::UnexpectedToken {
                                expected: vec!["IN, OUT, or INOUT".to_string()],
                                found: self.peek().cloned().unwrap_or_else(|| self.create_eof_token()),
                            })?;
                        let intent = match intent_str.to_uppercase().as_str() {
                            "IN" => Intent::In,
                            "OUT" => Intent::Out,
                            "INOUT" => Intent::InOut,
                            _ => return Err(ParseError::InvalidSyntax {
                                message: format!("Invalid INTENT: {}", intent_str),
                                line: self.current_line(),
                                column: self.current_column(),
                            }),
                        };
                        self.consume_token(&TokenType::RightParen)?;
                        type_attributes.push(Attribute::Intent(intent));
                    } else if self.check_token(&TokenType::Allocatable) {
                        self.advance();
                        type_attributes.push(Attribute::Allocatable);
                    } else if self.check_token(&TokenType::Dimension) {
                        self.advance();
                        self.consume_token(&TokenType::LeftParen)?;
                        let dims = self.parse_dimension_list()?;
                        self.consume_token(&TokenType::RightParen)?;
                        type_attributes.push(Attribute::Dimension(dims));
                    } else {
                        // Unknown attribute, break and let variable parsing handle it
                        break;
                    }
                }
                
                // Parse variable declarations (:: x, y)
                let vars = self.parse_variable_declarations_with_attributes(&type_spec, type_attributes)?;
                for var in vars {
                    declarations.push(var);
                }
            } else {
                break;
            }
        }
        
        Ok(declarations)
    }
    
    /// Parse variable declarations with a type specification.
    #[allow(dead_code)] // Legacy method, kept for compatibility
    fn parse_variable_declarations(&mut self, type_spec: &TypeSpec) -> ParseResult<Vec<Spanned<Declaration>>> {
        self.parse_variable_declarations_with_attributes(type_spec, Vec::new())
    }
    
    /// Parse variable declarations with a type specification and attributes.
    fn parse_variable_declarations_with_attributes(&mut self, type_spec: &TypeSpec, type_attributes: Vec<Attribute>) -> ParseResult<Vec<Spanned<Declaration>>> {
        let mut declarations = Vec::new();
        
        // Consume the double colon (::) if present
        if self.check_token(&TokenType::DoubleColon) {
            self.advance();
        }
        
        loop {
            let name = self.parse_identifier()
                .ok_or_else(|| ParseError::UnexpectedToken {
                    expected: vec!["identifier".to_string()],
                    found: self.peek().cloned().unwrap_or_else(|| self.create_eof_token()),
            })?;
            
            // Use the type-level attributes (intent, etc.) for this variable
            let attributes = type_attributes.clone();
            
            // Parse any remaining attributes after the variable name
            // (This is less common but possible)
            
            // Optional initializer
            let initializer = if self.check_token(&TokenType::Equals) {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            };
            
            let span = self.create_span(0, 0, 1, 1);
            declarations.push(Spanned::new(
                Declaration::Variable {
                    name,
                    type_spec: type_spec.clone(),
                    attributes,
                    initializer,
                },
                span,
            ));
            
            if !self.check_token(&TokenType::Comma) {
                break;
            }
            self.advance(); // consume comma
        }
        
        Ok(declarations)
    }
    
    /// Parse attributes (::, DIMENSION, INTENT, etc.).
    #[allow(dead_code)] // Legacy method, functionality moved to declaration parsing
    fn parse_attributes(&mut self) -> ParseResult<Vec<Attribute>> {
        let mut attributes = Vec::new();
        
        // Skip :: if present
        if self.check_token(&TokenType::DoubleColon) {
            self.advance();
        }
        
        while !self.is_at_end() && !self.check_token(&TokenType::Equals) && !self.check_token(&TokenType::Comma) {
            if self.check_token(&TokenType::Allocatable) {
                self.advance();
                attributes.push(Attribute::Allocatable);
            } else if self.check_token(&TokenType::Intent) {
                self.advance();
                self.consume_token(&TokenType::LeftParen)?;
                let intent_str = self.parse_identifier()
                    .ok_or_else(|| ParseError::UnexpectedToken {
                        expected: vec!["IN, OUT, or INOUT".to_string()],
                        found: self.peek().cloned().unwrap_or_else(|| self.create_eof_token()),
                    })?;
                let intent = match intent_str.to_uppercase().as_str() {
                    "IN" => Intent::In,
                    "OUT" => Intent::Out,
                    "INOUT" => Intent::InOut,
                    _ => return Err(ParseError::InvalidSyntax {
                        message: format!("Invalid INTENT: {}", intent_str),
                        line: self.current_line(),
                        column: self.current_column(),
                    }),
                };
                self.consume_token(&TokenType::RightParen)?;
                attributes.push(Attribute::Intent(intent));
            } else if self.check_token(&TokenType::Dimension) {
                self.advance();
                self.consume_token(&TokenType::LeftParen)?;
                let dims = self.parse_dimension_list()?;
                self.consume_token(&TokenType::RightParen)?;
                attributes.push(Attribute::Dimension(dims));
            } else {
                break;
            }
        }
        
        Ok(attributes)
    }
    
    /// Parse dimension list.
    fn parse_dimension_list(&mut self) -> ParseResult<Vec<Spanned<Expression>>> {
        let mut dims = Vec::new();
        
        loop {
            dims.push(self.parse_expression()?);
            if !self.check_token(&TokenType::Comma) {
                break;
            }
            self.advance();
        }
        
        Ok(dims)
    }
    
    /// Parse argument list.
    fn parse_argument_list(&mut self) -> ParseResult<Vec<Argument>> {
        let mut args = Vec::new();
        
        if self.check_token(&TokenType::RightParen) {
            return Ok(args);
        }
        
        loop {
            let name = self.parse_identifier()
                .ok_or_else(|| ParseError::UnexpectedToken {
                    expected: vec!["identifier".to_string()],
                    found: self.peek().cloned().unwrap_or_else(|| self.create_eof_token()),
                })?;
            
            // Parse INTENT if present
            let intent = if self.check_token(&TokenType::Intent) {
                self.advance();
                self.consume_token(&TokenType::LeftParen)?;
                let intent_str = self.parse_identifier()
                    .ok_or_else(|| ParseError::UnexpectedToken {
                        expected: vec!["IN, OUT, or INOUT".to_string()],
                        found: self.peek().cloned().unwrap_or_else(|| self.create_eof_token()),
                    })?;
                let intent = match intent_str.to_uppercase().as_str() {
                    "IN" => Intent::In,
                    "OUT" => Intent::Out,
                    "INOUT" => Intent::InOut,
                    _ => return Err(ParseError::InvalidSyntax {
                        message: format!("Invalid INTENT: {}", intent_str),
                        line: self.current_line(),
                        column: self.current_column(),
                    }),
                };
                self.consume_token(&TokenType::RightParen)?;
                Some(intent)
            } else {
                None
            };
            
            args.push(Argument {
                name,
                intent,
                optional: false, // TODO: Parse OPTIONAL keyword
            });
            
            if !self.check_token(&TokenType::Comma) {
                break;
            }
            self.advance();
        }
        
        Ok(args)
    }
    
    /// Parse executable statements.
    fn parse_executable_statements(&mut self) -> ParseResult<Vec<Spanned<Statement>>> {
        let mut statements = Vec::new();
        
        // Stop when we hit END tokens or end of file
        while !self.is_at_end() {
            // Check for END tokens first - these mark the end of executable section
            if matches!(
                self.peek().map(|t| &t.token_type),
                Some(TokenType::EndProgram)
                    | Some(TokenType::EndSubroutine)
                    | Some(TokenType::EndFunction)
                    | Some(TokenType::EndModule)
                    | Some(TokenType::Contains)
            ) {
                break;
            }
            
            if self.is_executable_statement() {
                if let Some(statement) = self.parse_statement_opt()? {
                    statements.push(statement);
                } else {
                    break;
                }
            } else {
                break; // Not an executable statement, stop parsing
            }
        }
        
        Ok(statements)
    }
    
    /// Check if current token is an executable statement.
    /// Note: This excludes END tokens which are handled separately.
    fn is_executable_statement(&self) -> bool {
        // Check for explicit statement keywords (but not END tokens - those are handled separately)
        if matches!(
            self.peek().map(|t| &t.token_type),
            Some(TokenType::If)
                | Some(TokenType::Do)
                | Some(TokenType::Read)
                | Some(TokenType::Write)
                | Some(TokenType::Print)
                | Some(TokenType::Return)
                | Some(TokenType::Stop)
                | Some(TokenType::Continue)
                | Some(TokenType::Cycle)
                | Some(TokenType::Exit)
                | Some(TokenType::Select)
                | Some(TokenType::Case)
                | Some(TokenType::EndIf)
                | Some(TokenType::EndDo)
                | Some(TokenType::EndSelect)
                | Some(TokenType::DoWhile)
        ) {
            return true;
        }
        
        // Check for DO WHILE: DO followed by WHILE
        if let Some(TokenType::Do) = self.peek().map(|t| &t.token_type) {
            // Peek ahead to see if next non-trivial token is WHILE
            let mut idx = self.current + 1;
            while idx < self.tokens.len() {
                if let Some(token) = self.tokens.get(idx) {
                    if !token.is_trivial() {
                        if matches!(token.token_type, TokenType::DoWhile) {
                            return true; // DO WHILE
                        } else if let TokenType::Identifier(ref name) = &token.token_type {
                            if name.to_uppercase() == "WHILE" {
                                return true; // DO WHILE
                            }
                        }
                        break; // Not DO WHILE
                    }
                }
                idx += 1;
            }
        }
        
        // Check for assignment: identifier followed by =
        if let Some(TokenType::Identifier(_)) = self.peek().map(|t| &t.token_type) {
            // Peek ahead to see if next non-trivial token is =
            // We need to skip past the current identifier and any whitespace
            // Start from current position and skip trivial tokens
            let mut idx = self.current;
            // First, skip to the current non-trivial token (which is the identifier)
            while idx < self.tokens.len() {
                if let Some(token) = self.tokens.get(idx) {
                    if !token.is_trivial() {
                        break; // Found the identifier
                    }
                }
                idx += 1;
            }
            // Now skip past the identifier and any whitespace
            idx += 1;
            while idx < self.tokens.len() {
                if let Some(token) = self.tokens.get(idx) {
                    if !token.is_trivial() {
                        if matches!(token.token_type, TokenType::Equals) {
                            return true; // This is an assignment
                        }
                        break; // Not an assignment
                    }
                }
                idx += 1;
            }
        }
        
        false
    }
    
    /// Parse a statement (if present).
    fn parse_statement_opt(&mut self) -> ParseResult<Option<Spanned<Statement>>> {
        if self.is_at_end() {
            return Ok(None);
        }
        
        let token = self.peek().ok_or_else(|| ParseError::UnexpectedEof {
            expected: vec!["statement".to_string()],
        })?;
        
        match &token.token_type {
            TokenType::If => {
                self.parse_if_statement().map(Some)
            }
            TokenType::Do => {
                // Check if next non-trivial token is DoWhile or identifier "while"
                // First, find where the DO token actually is (skip whitespace)
                let mut do_idx = self.current;
                while do_idx < self.tokens.len() {
                    if let Some(token) = self.tokens.get(do_idx) {
                        if !token.is_trivial()
                            && matches!(token.token_type, TokenType::Do) {
                                break; // Found DO token
                            }
                    }
                    do_idx += 1;
                }
                
                // Now check the token after DO (skip whitespace)
                let mut idx = do_idx + 1;
                let mut found_while = false;
                while idx < self.tokens.len() {
                    if let Some(token) = self.tokens.get(idx) {
                        if !token.is_trivial() {
                            if matches!(token.token_type, TokenType::DoWhile) {
                                found_while = true;
                            } else if let TokenType::Identifier(ref name) = &token.token_type {
                                if name.to_uppercase() == "WHILE" {
                                    found_while = true;
                                }
                            }
                            break;
                        }
                    }
                    idx += 1;
                }
                if found_while {
                    // Consume DO token first, then parse DO WHILE
                    self.advance();
                    self.parse_do_while_statement().map(Some)
                } else {
                    self.parse_do_statement().map(Some)
                }
            }
            TokenType::Select => {
                self.parse_select_case_statement().map(Some)
            }
            TokenType::Read => {
                self.parse_read_statement().map(Some)
            }
            TokenType::Write => {
                self.parse_write_statement().map(Some)
            }
            TokenType::Print => {
                self.parse_print_statement().map(Some)
            }
            // Note: CALL is not a separate token, it's an identifier followed by parentheses
            // This will be handled in assignment parsing
            TokenType::Return => {
                self.parse_return_statement().map(Some)
            }
            TokenType::Stop => {
                self.parse_stop_statement().map(Some)
            }
            TokenType::Continue => {
                self.advance();
                Ok(Some(Spanned::new(Statement::Continue, self.create_span(0, 0, 1, 1))))
            }
            TokenType::Cycle => {
                self.advance();
                Ok(Some(Spanned::new(Statement::Cycle, self.create_span(0, 0, 1, 1))))
            }
            TokenType::Exit => {
                self.advance();
                Ok(Some(Spanned::new(Statement::Exit, self.create_span(0, 0, 1, 1))))
            }
            TokenType::Identifier(_) => {
                // Assignment statement
                self.parse_assignment_statement().map(Some)
            }
            _ => Ok(None),
        }
    }
    
    /// Parse an assignment statement.
    fn parse_assignment_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        // Parse the variable (left-hand side) - this should NOT consume the = sign
        // We need to parse an expression but stop before = (which is assignment, not equality)
        let variable = self.parse_expression_without_equals()?;
        self.consume_token(&TokenType::Equals)?;
        let value = self.parse_expression()?;
        
        Ok(Spanned::new(
            Statement::Assignment {
                variable,
                value,
            },
            self.create_span(0, 0, 1, 1),
        ))
    }
    
    /// Parse an expression but stop before = (assignment operator).
    /// This is used for the left-hand side of assignments.
    fn parse_expression_without_equals(&mut self) -> ParseResult<Spanned<Expression>> {
        self.parse_binary_expression_without_equals(0)
    }
    
    /// Parse binary expression with precedence, but stop before =.
    fn parse_binary_expression_without_equals(&mut self, precedence: u8) -> ParseResult<Spanned<Expression>> {
        let mut left = self.parse_unary_expression()?;
        
        loop {
            // Don't parse = as a binary operator here - it's assignment, not equality
            if let Some(op) = self.parse_binary_operator_without_equals() {
                let op_precedence = self.get_operator_precedence(&op);
                if op_precedence < precedence {
                    break;
                }
                self.advance(); // consume operator
                let right = self.parse_binary_expression_without_equals(op_precedence + 1)?;
                left = Spanned::new(
                    Expression::Binary {
                        operator: op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    self.create_span(0, 0, 1, 1),
                );
            } else {
                break;
            }
        }
        
        Ok(left)
    }
    
    /// Parse binary operator, but exclude = (which is assignment, not equality in this context).
    fn parse_binary_operator_without_equals(&self) -> Option<BinaryOp> {
        let token_type = self.peek().map(|t| &t.token_type)?;
        match token_type {
            TokenType::Plus => Some(BinaryOp::Add),
            TokenType::Minus => Some(BinaryOp::Subtract),
            TokenType::Multiply => Some(BinaryOp::Multiply),
            TokenType::Divide => Some(BinaryOp::Divide),
            TokenType::Power => Some(BinaryOp::Power),
            // Skip Equals - that's for assignment, not comparison
            TokenType::NotEquals => Some(BinaryOp::NotEqual),
            TokenType::LessThan => Some(BinaryOp::LessThan),
            TokenType::LessOrEqual => Some(BinaryOp::LessOrEqual),
            TokenType::GreaterThan => Some(BinaryOp::GreaterThan),
            TokenType::GreaterOrEqual => Some(BinaryOp::GreaterOrEqual),
            TokenType::And => Some(BinaryOp::And),
            TokenType::Or => Some(BinaryOp::Or),
            TokenType::Eqv => Some(BinaryOp::Eqv),
            TokenType::Neqv => Some(BinaryOp::Neqv),
            _ => None,
        }
    }
    
    /// Parse an IF statement.
    fn parse_if_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        self.consume_token(&TokenType::If)?;
        self.consume_token(&TokenType::LeftParen)?;
        let condition = self.parse_expression()?;
        self.consume_token(&TokenType::RightParen)?;
        self.consume_token(&TokenType::Then)?;
        
        let then_statements = self.parse_executable_statements()?;
        
        let mut else_if_clauses = Vec::new();
        let mut else_statements = None;
        
        while self.check_token(&TokenType::ElseIf) || 
              (self.check_token(&TokenType::Else) && !self.check_token_after(&TokenType::If, 1)) {
            if self.check_token(&TokenType::ElseIf) {
                self.advance();
                self.consume_token(&TokenType::LeftParen)?;
                let condition = self.parse_expression()?;
                self.consume_token(&TokenType::RightParen)?;
                self.consume_token(&TokenType::Then)?;
                let statements = self.parse_executable_statements()?;
                else_if_clauses.push(ElseIfClause {
                    condition,
                    statements,
                });
            } else if self.check_token(&TokenType::Else) {
                self.advance();
                else_statements = Some(self.parse_executable_statements()?);
                break;
            } else {
                break;
            }
        }
        
        if self.check_token(&TokenType::EndIf) {
            self.advance();
        }
        
        Ok(Spanned::new(
            Statement::If {
                condition,
                then_statements,
                else_statements,
                else_if_clauses,
            },
            self.create_span(0, 0, 1, 1),
        ))
    }
    
    /// Parse a DO statement.
    fn parse_do_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        self.consume_token(&TokenType::Do)?;
        
        // Optional variable and loop bounds
        let variable = self.parse_identifier_opt();
        let (start, end, step) = if variable.is_some() && self.check_token(&TokenType::Equals) {
            self.advance();
            let start = Some(self.parse_expression()?);
            self.consume_token(&TokenType::Comma)?;
            let end = Some(self.parse_expression()?);
            let step = if self.check_token(&TokenType::Comma) {
                self.advance();
                Some(self.parse_expression()?)
            } else {
                None
            };
            (start, end, step)
        } else {
            (None, None, None)
        };
        
        let statements = self.parse_executable_statements()?;
        
        if self.check_token(&TokenType::EndDo) {
            self.advance();
        }
        
        Ok(Spanned::new(
            Statement::Do {
                variable,
                start,
                end,
                step,
                statements,
            },
            self.create_span(0, 0, 1, 1),
        ))
    }
    
    /// Parse a DO WHILE statement.
    fn parse_do_while_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        // DO token already consumed, now consume WHILE or DoWhile token
        if self.check_token(&TokenType::DoWhile) {
            self.advance();
        } else if let Some(token) = self.peek() {
            if let TokenType::Identifier(ref name) = token.token_type {
                if name.to_uppercase() == "WHILE" {
                    self.advance();
                } else {
                    return Err(ParseError::UnexpectedToken {
                        expected: vec!["WHILE".to_string()],
                        found: token.clone(),
                    });
                }
            } else {
                return Err(ParseError::UnexpectedToken {
                    expected: vec!["WHILE".to_string()],
                    found: token.clone(),
                });
            }
        } else {
            return Err(ParseError::UnexpectedEof {
                expected: vec!["WHILE".to_string()],
            });
        }
        
        self.consume_token(&TokenType::LeftParen)?;
        let condition = self.parse_expression()?;
        self.consume_token(&TokenType::RightParen)?;
        
        let statements = self.parse_executable_statements()?;
        
        if self.check_token(&TokenType::EndDo) {
            self.advance();
        }
        
        Ok(Spanned::new(
            Statement::DoWhile {
                condition,
                statements,
            },
            self.create_span(0, 0, 1, 1),
        ))
    }
    
    /// Parse a SELECT CASE statement.
    fn parse_select_case_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        self.consume_token(&TokenType::Select)?;
        self.consume_token(&TokenType::Case)?;
        self.consume_token(&TokenType::LeftParen)?;
        let expression = self.parse_expression()?;
        self.consume_token(&TokenType::RightParen)?;
        
        let mut cases = Vec::new();
        let mut default_case = None;
        
        while !self.is_at_end() {
            if self.check_token(&TokenType::Case) {
                self.advance();
                let selector = self.parse_case_selector()?;
                let statements = self.parse_executable_statements()?;
                cases.push(CaseClause {
                    selector,
                    statements,
                });
            } else if self.check_token(&TokenType::Default) {
                self.advance();
                default_case = Some(self.parse_executable_statements()?);
            } else if self.check_token(&TokenType::EndSelect) {
                break;
            } else {
                break;
            }
        }
        
        if self.check_token(&TokenType::EndSelect) {
            self.advance();
        }
        
        Ok(Spanned::new(
            Statement::SelectCase {
                expression,
                cases,
                default: default_case,
            },
            self.create_span(0, 0, 1, 1),
        ))
    }
    
    /// Parse a CASE selector.
    fn parse_case_selector(&mut self) -> ParseResult<CaseSelector> {
        if self.check_token(&TokenType::Default) {
            self.advance();
            return Ok(CaseSelector::Default);
        }
        
        // CASE selector can be: value, (value), (value1:value2), etc.
        // Handle parentheses if present
        let has_paren = self.check_token(&TokenType::LeftParen);
        if has_paren {
            self.advance(); // consume (
        }
        
        let start = self.parse_expression()?;
        
        // Check for range (start:end)
        if self.check_token(&TokenType::Colon) {
            self.advance(); // consume :
            let end = self.parse_expression()?;
            // If we had opening paren, consume closing paren
            if has_paren {
                self.consume_token(&TokenType::RightParen)?;
            }
            Ok(CaseSelector::Range {
                start,
                end,
            })
        } else {
            // Single value - if we had opening paren, consume closing paren
            if has_paren {
                self.consume_token(&TokenType::RightParen)?;
            }
            Ok(CaseSelector::Value(start))
        }
    }
    
    /// Parse a READ statement.
    fn parse_read_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        self.consume_token(&TokenType::Read)?;
        
        // Simplified: READ(*,*) or READ *, variables
        let unit = None;
        let format = None;
        let iostat = None;
        let iomsg = None;
        
        // Handle READ *, ... format
        if self.check_token(&TokenType::Multiply) {
            self.advance(); // consume *
            if self.check_token(&TokenType::Comma) {
                self.advance(); // consume comma
            }
        }
        
        let mut variables = Vec::new();
        if !self.is_at_end() && !self.check_token(&TokenType::Eof) {
            loop {
                variables.push(self.parse_expression()?);
                if !self.check_token(&TokenType::Comma) {
                    break;
                }
                self.advance();
            }
        }
        
        Ok(Spanned::new(
            Statement::Read {
                unit,
                format,
                iostat,
                iomsg,
                variables,
            },
            self.create_span(0, 0, 1, 1),
        ))
    }
    
    /// Parse a WRITE statement.
    fn parse_write_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        self.consume_token(&TokenType::Write)?;
        
        // Simplified: WRITE(*,*) or WRITE(*, *) expressions
        let unit = None;
        let format = None;
        let iostat = None;
        let iomsg = None;
        
        // Handle WRITE(*,*) format - skip parentheses and asterisks
        if self.check_token(&TokenType::LeftParen) {
            self.advance(); // consume (
            if self.check_token(&TokenType::Multiply) {
                self.advance(); // consume *
            }
            if self.check_token(&TokenType::Comma) {
                self.advance(); // consume ,
            }
            if self.check_token(&TokenType::Multiply) {
                self.advance(); // consume *
            }
            if self.check_token(&TokenType::RightParen) {
                self.advance(); // consume )
            }
        }
        
        let mut expressions = Vec::new();
        if !self.is_at_end() && !self.check_token(&TokenType::Eof) {
            loop {
                expressions.push(self.parse_expression()?);
                if !self.check_token(&TokenType::Comma) {
                    break;
                }
                self.advance();
            }
        }
        
        Ok(Spanned::new(
            Statement::Write {
                unit,
                format,
                iostat,
                iomsg,
                expressions,
            },
            self.create_span(0, 0, 1, 1),
        ))
    }
    
    /// Parse a PRINT statement.
    fn parse_print_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        self.consume_token(&TokenType::Print)?;
        
        let format = None;
        let mut expressions = Vec::new();
        
        // Handle PRINT *, ... format
        if self.check_token(&TokenType::Multiply) {
            self.advance(); // consume *
            if self.check_token(&TokenType::Comma) {
                self.advance(); // consume comma
            }
        }
        
        if !self.is_at_end() && !self.check_token(&TokenType::Eof) {
            loop {
                expressions.push(self.parse_expression()?);
                if !self.check_token(&TokenType::Comma) {
                    break;
                }
                self.advance();
            }
        }
        
        Ok(Spanned::new(
            Statement::Print {
                format,
                expressions,
            },
            self.create_span(0, 0, 1, 1),
        ))
    }
    
    // Note: CALL statements are parsed as function calls in expressions
    
    /// Parse a RETURN statement.
    fn parse_return_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        self.consume_token(&TokenType::Return)?;
        let expression = if !self.is_at_end() && !self.check_token(&TokenType::Eof) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        Ok(Spanned::new(
            Statement::Return {
                expression,
            },
            self.create_span(0, 0, 1, 1),
        ))
    }
    
    /// Parse a STOP statement.
    fn parse_stop_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        self.consume_token(&TokenType::Stop)?;
        let code = if !self.is_at_end() && !self.check_token(&TokenType::Eof) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        Ok(Spanned::new(
            Statement::Stop {
                code,
            },
            self.create_span(0, 0, 1, 1),
        ))
    }
    
    /// Parse an expression.
    fn parse_expression(&mut self) -> ParseResult<Spanned<Expression>> {
        self.parse_binary_expression(0)
    }
    
    /// Parse binary expression with precedence.
    fn parse_binary_expression(&mut self, precedence: u8) -> ParseResult<Spanned<Expression>> {
        let mut left = self.parse_unary_expression()?;
        
        loop {
            if let Some(op) = self.parse_binary_operator() {
                let op_precedence = self.get_operator_precedence(&op);
                if op_precedence < precedence {
                    break;
                }
                self.advance(); // consume operator
                let right = self.parse_binary_expression(op_precedence + 1)?;
                left = Spanned::new(
                    Expression::Binary {
                        operator: op,
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    self.create_span(0, 0, 1, 1),
                );
            } else {
                break;
            }
        }
        
        Ok(left)
    }
    
    /// Parse unary expression.
    fn parse_unary_expression(&mut self) -> ParseResult<Spanned<Expression>> {
        if self.check_token(&TokenType::Minus) {
            self.advance();
            let operand = self.parse_unary_expression()?;
            Ok(Spanned::new(
                Expression::Unary {
                    operator: UnaryOp::Minus,
                    operand: Box::new(operand),
                },
                self.create_span(0, 0, 1, 1),
            ))
        } else if self.check_token(&TokenType::Plus) {
            self.advance();
            let operand = self.parse_unary_expression()?;
            Ok(Spanned::new(
                Expression::Unary {
                    operator: UnaryOp::Plus,
                    operand: Box::new(operand),
                },
                self.create_span(0, 0, 1, 1),
            ))
        } else if self.check_token(&TokenType::Not) {
            self.advance();
            let operand = self.parse_unary_expression()?;
            Ok(Spanned::new(
                Expression::Unary {
                    operator: UnaryOp::Not,
                    operand: Box::new(operand),
                },
                self.create_span(0, 0, 1, 1),
            ))
        } else {
            self.parse_primary_expression()
        }
    }
    
    /// Parse primary expression.
    fn parse_primary_expression(&mut self) -> ParseResult<Spanned<Expression>> {
        let token_type = self.peek().ok_or_else(|| ParseError::UnexpectedEof {
            expected: vec!["expression".to_string()],
        })?.token_type.clone();
        
        match token_type {
            TokenType::IntegerLiteral(s) => {
                self.advance();
                Ok(Spanned::new(
                    Expression::Literal(Literal::integer(&s)),
                    self.create_span(0, 0, 1, 1),
                ))
            }
            TokenType::RealLiteral(s) => {
                self.advance();
                Ok(Spanned::new(
                    Expression::Literal(Literal::real(&s)),
                    self.create_span(0, 0, 1, 1),
                ))
            }
            TokenType::CharacterLiteral(s) => {
                self.advance();
                Ok(Spanned::new(
                    Expression::Literal(Literal::character(&s)),
                    self.create_span(0, 0, 1, 1),
                ))
            }
            TokenType::LogicalLiteral(s) => {
                self.advance();
                let value = s == "true";
                Ok(Spanned::new(
                    Expression::Literal(Literal::logical(value)),
                    self.create_span(0, 0, 1, 1),
                ))
            }
            TokenType::Identifier(name) => {
                self.advance();
                // Check if it's a function call or array access
                if self.check_token(&TokenType::LeftParen) {
                    self.advance();
                    let args = self.parse_expression_list()?;
                    self.consume_token(&TokenType::RightParen)?;
                    // Heuristic: if it looks like array access (single integer expression), treat as array
                    // Otherwise treat as function call
                    if args.len() == 1 && matches!(args[0].node, Expression::Literal(_) | Expression::Variable(_)) {
                        Ok(Spanned::new(
                            Expression::ArrayElement {
                                variable: name,
                                subscripts: args,
                            },
                            self.create_span(0, 0, 1, 1),
                        ))
                    } else {
                        Ok(Spanned::new(
                            Expression::FunctionCall {
                                name,
                                arguments: args,
                            },
                            self.create_span(0, 0, 1, 1),
                        ))
                    }
                } else {
                    Ok(Spanned::new(
                        Expression::Variable(name),
                        self.create_span(0, 0, 1, 1),
                    ))
                }
            }
            TokenType::LeftParen => {
                self.advance();
                // Check if it's an array constructor [expr1, expr2, ...]
                let mut expressions = Vec::new();
                if !self.check_token(&TokenType::RightParen) {
                    loop {
                        expressions.push(self.parse_expression()?);
                        if !self.check_token(&TokenType::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                self.consume_token(&TokenType::RightParen)?;
                
                // If multiple expressions, treat as array constructor
                if expressions.len() > 1 {
                    Ok(Spanned::new(
                        Expression::ArrayConstructor(expressions),
                        self.create_span(0, 0, 1, 1),
                    ))
                } else if expressions.len() == 1 {
                    Ok(expressions.into_iter().next().unwrap())
                } else {
                    // Empty parentheses - just return the expression wrapped
                    Ok(Spanned::new(
                        Expression::Literal(Literal::integer("0")),
                        self.create_span(0, 0, 1, 1),
                    ))
                }
            }
            _ => {
                let token = self.peek().cloned().unwrap_or_else(|| self.create_eof_token());
                Err(ParseError::UnexpectedToken {
                    expected: vec!["expression".to_string()],
                    found: token,
                })
            },
        }
    }
    
    /// Parse expression list.
    fn parse_expression_list(&mut self) -> ParseResult<Vec<Spanned<Expression>>> {
        let mut expressions = Vec::new();
        
        if self.check_token(&TokenType::RightParen) {
            return Ok(expressions);
        }
        
        loop {
            expressions.push(self.parse_expression()?);
            if !self.check_token(&TokenType::Comma) {
                break;
            }
            self.advance();
        }
        
        Ok(expressions)
    }
    
    /// Parse binary operator.
    fn parse_binary_operator(&self) -> Option<BinaryOp> {
        let token_type = self.peek().map(|t| &t.token_type)?;
        match token_type {
            TokenType::Plus => Some(BinaryOp::Add),
            TokenType::Minus => Some(BinaryOp::Subtract),
            TokenType::Multiply => Some(BinaryOp::Multiply),
            TokenType::Divide => Some(BinaryOp::Divide),
            TokenType::Power => Some(BinaryOp::Power),
            TokenType::Equals => Some(BinaryOp::Equal),
            TokenType::NotEquals => Some(BinaryOp::NotEqual),
            TokenType::LessThan => Some(BinaryOp::LessThan),
            TokenType::LessOrEqual => Some(BinaryOp::LessOrEqual),
            TokenType::GreaterThan => Some(BinaryOp::GreaterThan),
            TokenType::GreaterOrEqual => Some(BinaryOp::GreaterOrEqual),
            TokenType::And => Some(BinaryOp::And),
            TokenType::Or => Some(BinaryOp::Or),
            TokenType::Eqv => Some(BinaryOp::Eqv),
            TokenType::Neqv => Some(BinaryOp::Neqv),
            _ => None,
        }
    }
    
    /// Get operator precedence.
    fn get_operator_precedence(&self, op: &BinaryOp) -> u8 {
        match op {
            BinaryOp::Power => 7,
            BinaryOp::Multiply | BinaryOp::Divide => 6,
            BinaryOp::Add | BinaryOp::Subtract => 5,
            BinaryOp::Concat => 4,
            BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::LessThan 
                | BinaryOp::LessOrEqual | BinaryOp::GreaterThan | BinaryOp::GreaterOrEqual => 3,
            // Not is a unary operator, not binary
            BinaryOp::And | BinaryOp::Or => 1,
            BinaryOp::Eqv | BinaryOp::Neqv => 0,
        }
    }
    
    /// Parse type specification (optional).
    fn parse_type_spec_opt(&mut self) -> Option<TypeSpec> {
        if self.check_token(&TokenType::Integer) {
            self.advance();
            Some(TypeSpec::Integer { kind: None })
        } else if self.check_token(&TokenType::Real) {
            self.advance();
            Some(TypeSpec::Real { kind: None })
        } else if self.check_token(&TokenType::DoublePrecision) {
            self.advance();
            Some(TypeSpec::DoublePrecision)
        } else if self.check_token(&TokenType::Complex) {
            self.advance();
            Some(TypeSpec::Complex { kind: None })
        } else if self.check_token(&TokenType::Character) {
            self.advance();
            
            // Parse CHARACTER length: character(len=10) or character(10)
            // We handle this carefully since we return Option, not Result
            let length = if self.check_token(&TokenType::LeftParen) {
                self.advance(); // consume (
                
                // Check for len= or just a number/expression
                let len_expr = if let Some(token) = self.peek() {
                    if let TokenType::Identifier(ref name) = &token.token_type {
                        if name.to_uppercase() == "LEN" {
                            // Handle len= syntax: character(len=10)
                            self.advance(); // consume len
                            if self.check_token(&TokenType::Equals) {
                                self.advance(); // consume =
                            }
                            // Try to parse expression
                            if let Ok(expr) = self.parse_expression() {
                                if self.check_token(&TokenType::RightParen) {
                                    self.advance(); // consume )
                                    Some(expr)
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            // Handle (10) syntax - just a number/expression
                            if let Ok(expr) = self.parse_expression() {
                                if self.check_token(&TokenType::RightParen) {
                                    self.advance(); // consume )
                                    Some(expr)
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                    } else {
                        // Handle (10) syntax - expression starts with something else
                        if let Ok(expr) = self.parse_expression() {
                            if self.check_token(&TokenType::RightParen) {
                                self.advance(); // consume )
                                Some(expr)
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    }
                } else {
                    None
                };
                
                // If we didn't parse successfully, try to consume the paren anyway
                if len_expr.is_none() && self.check_token(&TokenType::RightParen) {
                    self.advance();
                }
                
                len_expr
            } else {
                None
            };
            
            Some(TypeSpec::Character {
                length,
                kind: None,
            })
        } else if self.check_token(&TokenType::Logical) {
            self.advance();
            Some(TypeSpec::Logical { kind: None })
        } else {
            None
        }
    }
    
    /// Parse CONTAINS section.
    fn parse_contains(&mut self) -> ParseResult<ContainsSection> {
        self.consume_token(&TokenType::Contains)?;
        
        let mut procedures = Vec::new();
        while !self.is_at_end() {
            if self.check_token(&TokenType::Subroutine) {
                procedures.push(InternalProcedure::Subroutine(self.parse_subroutine()?));
            } else if self.check_token(&TokenType::Function) {
                procedures.push(InternalProcedure::Function(self.parse_function()?));
            } else {
                break;
            }
        }
        
        Ok(ContainsSection { internal_procedures: procedures })
    }
    
    // Helper methods
    
    fn peek(&self) -> Option<&Token> {
        let mut idx = self.current;
        while idx < self.tokens.len() {
            let token = &self.tokens[idx];
            if !token.is_trivial() {
                return Some(token);
            }
            idx += 1;
        }
        None
    }
    
    fn check_token(&self, token_type: &TokenType) -> bool {
        self.peek().is_some_and(|t| &t.token_type == token_type)
    }
    
    fn check_token_after(&self, token_type: &TokenType, offset: usize) -> bool {
        let mut idx = self.current + offset;
        while idx < self.tokens.len() {
            if let Some(token) = self.tokens.get(idx) {
                if !token.is_trivial() && token.token_type != TokenType::Eof {
                    return &token.token_type == token_type;
                }
            }
            idx += 1;
        }
        false
    }
    
    fn advance(&mut self) -> Option<&Token> {
        while self.current < self.tokens.len() {
            let token = &self.tokens[self.current];
            self.current += 1;
            if !token.is_trivial() {
                return Some(token);
            }
        }
        None
    }
    
    fn consume_token(&mut self, token_type: &TokenType) -> ParseResult<&Token> {
        let token = self.peek().ok_or_else(|| ParseError::UnexpectedEof {
            expected: vec![format!("{:?}", token_type)],
        })?;
        
        if &token.token_type == token_type {
            self.advance().ok_or_else(|| ParseError::UnexpectedEof {
                expected: vec![format!("{:?}", token_type)],
            })
        } else {
            Err(ParseError::UnexpectedToken {
                expected: vec![format!("{:?}", token_type)],
                found: token.clone(),
            })
        }
    }
    
    fn parse_identifier(&mut self) -> Option<String> {
        if let Some(token) = self.peek() {
            if let TokenType::Identifier(ref name) = token.token_type {
                let name = name.clone();
                self.advance();
                return Some(name);
            }
        }
        None
    }
    
    fn parse_identifier_opt(&mut self) -> Option<String> {
        self.parse_identifier()
    }
    
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || 
        self.tokens[self.current].token_type == TokenType::Eof
    }
    
    fn create_span(&self, start: usize, end: usize, line: usize, column: usize) -> Span {
        Span::new(start, end, line, column)
    }
    
    fn create_eof_token(&self) -> Token {
        Token::new(TokenType::Eof, String::new(), 0, 0, 0, 0)
    }
    
    fn current_line(&self) -> usize {
        self.peek().map(|t| t.line).unwrap_or(0)
    }
    
    fn current_column(&self) -> usize {
        self.peek().map(|t| t.column).unwrap_or(0)
    }
}

// Convenience functions

/// Parse FORTRAN source code into a Program AST.
pub fn parse(source: &str) -> ParseResult<Program> {
    let mut parser = Parser::new(source)?;
    parser.parse_program()
}

