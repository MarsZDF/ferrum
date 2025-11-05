/// FORTRAN token types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    // Program structure keywords
    Program,
    Subroutine,
    Function,
    Module,
    EndProgram,
    EndSubroutine,
    EndFunction,
    EndModule,
    Use,
    Contains,
    
    // Data type keywords
    Integer,
    Real,
    DoublePrecision,
    Complex,
    Character,
    Logical,
    Parameter,
    Allocatable,
    Dimension,
    Intent,
    
    // Control flow keywords
    If,
    Then,
    Else,
    ElseIf,
    EndIf,
    Do,
    EndDo,
    DoWhile,
    Select,
    Case,
    EndSelect,
    Default,
    Continue,
    Cycle,
    Exit,
    Stop,
    Return,
    
    // Declaration keywords
    Implicit,
    None,
    NoneExplicit,
    Public,
    Private,
    Save,
    Data,
    
    // I/O keywords
    Write,
    Read,
    Print,
    Open,
    Close,
    Format,
    Inquire,
    Backspace,
    Rewind,
    Endfile,
    
    // Operators
    Plus,              // +
    Minus,             // -
    Multiply,          // *
    Divide,            // /
    Power,             // **
    Equals,            // =
    NotEquals,         // /=
    LessThan,          // <
    LessOrEqual,       // <=
    GreaterThan,       // >
    GreaterOrEqual,    // >=
    And,               // .AND. or .and.
    Or,                // .OR. or .or.
    Not,               // .NOT. or .not.
    Eqv,               // .EQV. or .eqv.
    Neqv,              // .NEQV. or .neqv.
    
    // Punctuation
    LeftParen,         // (
    RightParen,        // )
    Comma,             // ,
    Semicolon,         // ;
    Colon,             // :
    Period,            // .
    DoubleColon,       // ::
    Assignment,        // =>
    
    // Literals
    IntegerLiteral(String),
    RealLiteral(String),
    ComplexLiteral(String),
    CharacterLiteral(String),
    LogicalLiteral(String),  // .TRUE. or .FALSE.
    
    // Identifiers
    Identifier(String),
    
    // Special constructs
    FormatSpec,        // Format specifier (e.g., '(I5)')
    
    // Comments
    Comment(String),
    
    // Whitespace (optionally preserved)
    Whitespace(String),
    
    // End of file
    Eof,
    
    // Unknown/error token
    Unknown(char),
}

/// Token with location information.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
    pub start: usize,
    pub end: usize,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: String,
        line: usize,
        column: usize,
        start: usize,
        end: usize,
    ) -> Self {
        Self {
            token_type,
            lexeme,
            line,
            column,
            start,
            end,
        }
    }
    
    /// Check if this token is trivial (whitespace or comment).
    pub fn is_trivial(&self) -> bool {
        matches!(self.token_type, TokenType::Whitespace(_) | TokenType::Comment(_))
    }
}

/// FORTRAN keywords (case-insensitive).
pub const KEYWORDS: &[(&str, TokenType)] = &[
    // Program structure
    ("PROGRAM", TokenType::Program),
    ("SUBROUTINE", TokenType::Subroutine),
    ("FUNCTION", TokenType::Function),
    ("MODULE", TokenType::Module),
    ("END", TokenType::EndProgram), // Context-dependent, will be resolved later
    ("END PROGRAM", TokenType::EndProgram),
    ("END SUBROUTINE", TokenType::EndSubroutine),
    ("END FUNCTION", TokenType::EndFunction),
    ("END MODULE", TokenType::EndModule),
    ("USE", TokenType::Use),
    ("CONTAINS", TokenType::Contains),
    
    // Data types
    ("INTEGER", TokenType::Integer),
    ("REAL", TokenType::Real),
    ("DOUBLE PRECISION", TokenType::DoublePrecision),
    ("COMPLEX", TokenType::Complex),
    ("CHARACTER", TokenType::Character),
    ("LOGICAL", TokenType::Logical),
    ("PARAMETER", TokenType::Parameter),
    ("ALLOCATABLE", TokenType::Allocatable),
    ("DIMENSION", TokenType::Dimension),
    ("INTENT", TokenType::Intent),
    
    // Control flow
    ("IF", TokenType::If),
    ("THEN", TokenType::Then),
    ("ELSE", TokenType::Else),
    ("ELSEIF", TokenType::ElseIf),
    ("ELSE IF", TokenType::ElseIf),
    ("ENDIF", TokenType::EndIf),
    ("END IF", TokenType::EndIf),
    ("DO", TokenType::Do),
    ("END DO", TokenType::EndDo),
    ("ENDDO", TokenType::EndDo),
    ("DO WHILE", TokenType::DoWhile),
    ("SELECT", TokenType::Select),
    ("CASE", TokenType::Case),
    ("END SELECT", TokenType::EndSelect),
    ("ENDSELECT", TokenType::EndSelect),
    ("DEFAULT", TokenType::Default),
    ("CONTINUE", TokenType::Continue),
    ("CYCLE", TokenType::Cycle),
    ("EXIT", TokenType::Exit),
    ("STOP", TokenType::Stop),
    ("RETURN", TokenType::Return),
    
    // Declarations
    ("IMPLICIT", TokenType::Implicit),
    ("NONE", TokenType::None),
    ("NONE", TokenType::NoneExplicit), // When used with IMPLICIT
    ("PUBLIC", TokenType::Public),
    ("PRIVATE", TokenType::Private),
    ("SAVE", TokenType::Save),
    ("DATA", TokenType::Data),
    
    // I/O
    ("WRITE", TokenType::Write),
    ("READ", TokenType::Read),
    ("PRINT", TokenType::Print),
    ("OPEN", TokenType::Open),
    ("CLOSE", TokenType::Close),
    ("FORMAT", TokenType::Format),
    ("INQUIRE", TokenType::Inquire),
    ("BACKSPACE", TokenType::Backspace),
    ("REWIND", TokenType::Rewind),
    ("ENDFILE", TokenType::Endfile),
    
    // Logical operators (case-insensitive)
    (".AND.", TokenType::And),
    (".OR.", TokenType::Or),
    (".NOT.", TokenType::Not),
    (".EQV.", TokenType::Eqv),
    (".NEQV.", TokenType::Neqv),
    
    // Logical literals - handled specially in lexer
];

/// Lookup a keyword (case-insensitive).
pub fn lookup_keyword(word: &str) -> Option<TokenType> {
    let upper = word.to_uppercase();
    for (keyword, token_type) in KEYWORDS {
        if keyword.to_uppercase() == upper {
            return Some(token_type.clone());
        }
    }
    None
}

