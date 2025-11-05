# Changelog

All notable changes to the Ferrum project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Enhanced FORTRAN parser CHARACTER type handling with length specification support (`character(len=10)`)
- Improved assignment statement parsing logic
- Better expression parsing for executable statements
- Fixed RESULT clause parsing in FUNCTION declarations
- Enhanced attribute parsing for variable declarations (INTENT, DIMENSION, ALLOCATABLE)

### Fixed
- Fixed double colon (`::`) parsing in FORTRAN type declarations
- Resolved clippy warnings across all crates
- Fixed assignment vs equality operator precedence in expression parsing
- Improved token position tracking in parser
- Better handling of whitespace and comment tokens

### Changed
- Refactored parser to use `parse_variable_declarations_with_attributes` for better attribute handling
- Improved error messages with more specific token expectations
- Enhanced SELECT CASE parsing with proper parentheses handling

## [0.1.0] - 2025-11-05

### Added

#### fortran-lexer
- ✅ Free-format FORTRAN lexing (FORTRAN 90+)
- ✅ Case-insensitive keyword recognition
- ✅ Comprehensive token types (keywords, identifiers, literals, operators, punctuation)
- ✅ Source location tracking (line, column, span)
- ✅ Error reporting with precise location information
- ✅ Format detection (fixed vs free format)
- ✅ Complete test suite with 20/20 tests passing
- Performance benchmarks with criterion

#### fortran-ast
- ✅ Complete AST representation of FORTRAN program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
- ✅ Declaration structures (variable declarations, type specifications, attributes)
- ✅ Expression trees (arithmetic, logical, comparison, function calls)
- ✅ Statement structures (IF, DO, SELECT CASE, I/O statements, etc.)
- ✅ Source span tracking for all nodes (`Spanned<T>`)
- ✅ Visitor pattern for AST traversal
- ✅ Optional serialization support (serde feature)

#### fortran-parser
- ✅ Parses FORTRAN program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
- ✅ Parses declarations (variable declarations, type specifications, attributes)
- ✅ Parses executable statements (IF, DO, READ, WRITE, PRINT, RETURN, STOP, etc.)
- ✅ Parses expressions (arithmetic, logical, comparison, function calls)
- ✅ Error reporting with location information
- ✅ Handles whitespace and comments gracefully
- ✅ Recursive descent parsing with precedence climbing
- Comprehensive integration test suite

#### Project Infrastructure
- ✅ Modular workspace structure with 3 focused crates
- ✅ GitHub Actions CI/CD pipeline
- ✅ Tests on stable, beta, and nightly Rust
- ✅ Cross-platform testing (Linux, Windows, macOS)
- ✅ Clippy linting and rustfmt formatting
- ✅ Comprehensive documentation with AI assistant guide
- ✅ Production readiness checklist
- ✅ MIT license

### Known Limitations
- Fixed-format FORTRAN lexer implementation is incomplete (marked as TODO)
- Some advanced FORTRAN constructs may not be fully supported yet
- Parser test coverage: 8/15 integration tests passing (with recent improvements)

### Technical Details
- Built with Rust 2021 edition
- Zero-copy tokenization where possible
- Comprehensive error handling with custom error types
- Source location tracking throughout the parsing pipeline
- Visitor pattern support for AST analysis tools

---

## Release Notes

### 0.1.0 Release
This is the initial release of Ferrum, providing a solid foundation for FORTRAN analysis tools. The lexer is production-ready with excellent test coverage, while the parser covers the core FORTRAN constructs needed for most analysis tasks.

**What's Working Well:**
- Tokenization of both simple and complex FORTRAN programs
- Parsing of basic program structures (PROGRAM, SUBROUTINE, FUNCTION)
- Variable declarations with type specifications
- Basic executable statements (IF, DO, I/O operations)
- Expression parsing with proper operator precedence

**Coming Next:**
- Complete fixed-format FORTRAN support
- Enhanced parser grammar coverage
- Analysis tools and utilities
- Language server protocol (LSP) support

### Development Status
- **fortran-lexer**: ✅ Ready for production use
- **fortran-ast**: ✅ Core structures complete
- **fortran-parser**: 🚧 Core functionality implemented, ongoing improvements

The project follows semantic versioning and maintains backwards compatibility within major versions.