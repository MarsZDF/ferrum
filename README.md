# Ferrum

A modular, open-source FORTRAN tooling ecosystem in Rust. Ferrum provides a collection of small, composable libraries that form the foundation for FORTRAN analysis, refactoring, and modernization tools.

## 🎯 Goals

- **Modular**: Small, focused crates that work together
- **Fast**: Built with Rust for performance
- **Composable**: Use what you need, combine as needed
- **Open Source**: MIT licensed
- **Production Ready**: Comprehensive error handling and testing

## 📦 Crates

### fortran-lexer
Fast, modular lexer for FORTRAN source code supporting both fixed-format and free-format FORTRAN.

**Features**:
- ✅ Free-format FORTRAN lexing (FORTRAN 90+)
- ✅ Case-insensitive keyword recognition
- ✅ Comprehensive token types (keywords, identifiers, literals, operators, punctuation)
- ✅ Source location tracking (line, column, span)
- ✅ Error reporting with precise location information
- ✅ Format detection (fixed vs free format)
- 🚧 Fixed-format FORTRAN lexing (in progress)

**Status**: ✅ Core functionality ready

[📖 Documentation](fortran-lexer/README.md) | [Examples](fortran-lexer/examples/)

### fortran-ast
Abstract Syntax Tree (AST) data structures for FORTRAN programs.

**Features**:
- ✅ Complete AST representation of FORTRAN program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
- ✅ Declaration structures (variable declarations, type specifications, attributes)
- ✅ Expression trees (arithmetic, logical, comparison, function calls)
- ✅ Statement structures (IF, DO, SELECT CASE, I/O statements, etc.)
- ✅ Source span tracking for all nodes
- ✅ Visitor pattern for AST traversal
- ✅ Optional serialization support (serde)

**Status**: ✅ Core structures defined

[📖 Documentation](fortran-ast/README.md)

### fortran-parser
Recursive descent parser that converts tokens into a structured AST.

**Features**:
- ✅ Parses FORTRAN program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
- ✅ Parses declarations (variable declarations, type specifications, attributes)
- ✅ Parses executable statements (IF, DO, READ, WRITE, PRINT, RETURN, STOP, etc.)
- ✅ Parses expressions (arithmetic, logical, comparison, function calls)
- ✅ Error reporting with location information
- ✅ Handles whitespace and comments gracefully

**Status**: ✅ Basic parsing implemented

[📖 Documentation](fortran-parser/README.md) | [Examples](fortran-parser/examples/)

## 🚀 Quick Start

### Installation

```bash
# Clone the repository
git clone https://github.com/MarsZDF/ferrum.git
cd ferrum

# Build all crates
cargo build --all
```

### Using the Lexer

```rust
use fortran_lexer::{tokenize, Format};

let source = r#"
program hello_world
    implicit none
    print *, 'Hello, World!'
end program hello_world
"#;

let tokens = tokenize(source, Format::FreeFormat)?;
for token in tokens {
    if !token.is_trivial() {
        println!("{:?} at line {}:{}", token.token_type, token.line, token.column);
    }
}
```

### Running Examples

```bash
# Run the lexer example
cd fortran-lexer && cargo run --example basic_tokenize
```

## 🏗️ Architecture

```
┌─────────────────────┐
│  FORTRAN Source     │
│  (.f, .f90, .f95)   │
└──────────┬──────────┘
           │
           v
┌─────────────────────┐
│  fortran-lexer      │ Tokenizes source code
│                     │ (fixed-format 🚧, free-format 🚧)
└──────────┬──────────┘
           │
           v
┌─────────────────────┐
│  fortran-parser     │ Parses tokens into AST
│                     │ (recursive descent)
└──────────┬──────────┘
           │
           v
┌─────────────────────┐
│   fortran-ast       │ AST data structures
│                     │ (with visitor pattern)
└──────────┬──────────┘
           │
           v
┌─────────────────────┐
│   Analyzers         │ Static analysis, refactoring, etc.
└─────────────────────┘
```

## 🧪 Development

### Prerequisites

- Rust 1.70+ (stable, beta, or nightly)
- Cargo (comes with Rust)

### Building

```bash
# Build all crates
cargo build --all

# Build a specific crate
cd fortran-lexer && cargo build
```

### Testing

```bash
# Run all tests
cargo test --all

# Run tests for a specific crate
cd fortran-lexer && cargo test

# Run with output
cargo test --all -- --nocapture
```

### Running Examples

```bash
# Run lexer example
cd fortran-lexer && cargo run --example basic_tokenize
```

### Linting and Formatting

```bash
# Format code
cargo fmt --all

# Run clippy
cargo clippy --all -- -D warnings
```

## 🚦 CI/CD

We use GitHub Actions for continuous integration:

- ✅ Tests on stable, beta, and nightly Rust
- ✅ Tests on Linux, Windows, and macOS
- ✅ Linting with clippy and rustfmt
- ✅ Builds examples and documentation
- ✅ All crates tested in the pipeline

See [`.github/workflows/ci.yml`](.github/workflows/ci.yml) for details.

## 📝 Contributing

Contributions are welcome! This project follows standard Rust conventions:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass (`cargo test --all`)
6. Run clippy and fix warnings (`cargo clippy --all`)
7. Format code (`cargo fmt --all`)
8. Update documentation as needed
9. Submit a pull request

### Development Guidelines

- Follow Rust naming conventions
- Write comprehensive tests
- Document public APIs with rustdoc
- Handle errors explicitly (use `Result` types)
- Keep crates focused and modular
- Use workspace dependencies where appropriate

## 📄 License

Licensed under the MIT License - see [LICENSE](LICENSE) for details.

## 🗺️ Roadmap

### Completed ✅
- [x] fortran-lexer - Free-format FORTRAN lexer
- [x] fortran-ast - Core AST structures
- [x] fortran-parser - Basic parser implementation
- [x] Comprehensive test suite for lexer
- [x] CI/CD pipeline setup

### In Progress 🚧
- [ ] fortran-lexer - Fixed-format FORTRAN lexer
- [ ] fortran-parser - Full FORTRAN grammar support

### Planned 📋
- [ ] fortran-analyzer-* - Analysis modules
  - [ ] Code quality metrics
  - [ ] Dead code detection
  - [ ] Performance analysis
  - [ ] Modernization suggestions
- [ ] Language server support (LSP)
- [ ] Formatter
- [ ] Refactoring tools
- [ ] REPL for FORTRAN exploration

## 🤝 Acknowledgments

This project aims to modernize FORTRAN tooling using Rust's excellent performance and safety guarantees. Special thanks to:

- The Rust community for excellent tooling and documentation
- FORTRAN maintainers for keeping scientific computing systems running
- Contributors and users of this project

## 📚 Additional Resources

- [FORTRAN Language Reference](https://gcc.gnu.org/onlinedocs/gfortran/)
- [Rust Book](https://doc.rust-lang.org/book/)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)

## 💬 Community

- Issues: [GitHub Issues](https://github.com/MarsZDF/ferrum/issues)
- Discussions: [GitHub Discussions](https://github.com/MarsZDF/ferrum/discussions)
- Pull Requests: [GitHub Pull Requests](https://github.com/MarsZDF/ferrum/pulls)

---

## 🤖 AI Assistant Guide

This section is designed to help AI coding assistants (like Cursor, Claude Code, GitHub Copilot) quickly understand and work with the Ferrum codebase.

### Project Structure

```
ferrum/
├── fortran-lexer/          # Tokenization layer
│   ├── src/
│   │   ├── lib.rs          # Main exports
│   │   ├── token.rs        # Token types and Token struct
│   │   ├── lexer.rs        # Lexer implementation (FreeFormatLexer)
│   │   └── error.rs        # LexError types
│   ├── tests/
│   └── examples/
├── fortran-ast/            # AST data structures
│   ├── src/
│   │   ├── lib.rs          # Main exports
│   │   ├── program.rs      # Program, ProgramUnit, MainProgram, etc.
│   │   ├── declaration.rs  # Declarations, TypeSpec, Attributes
│   │   ├── statement.rs    # Statements (IF, DO, SELECT CASE, etc.)
│   │   ├── expression.rs   # Expressions (arithmetic, logical, calls)
│   │   ├── span.rs         # Source location tracking
│   │   └── visitor.rs      # Visitor pattern for AST traversal
│   └── tests/
├── fortran-parser/         # Parsing layer
│   ├── src/
│   │   ├── lib.rs          # Main exports and parse() function
│   │   └── parser.rs       # Recursive descent parser (large file ~1500 lines)
│   ├── tests/
│   │   └── integration_tests.rs  # Comprehensive test suite
│   └── examples/
└── Cargo.toml              # Workspace configuration
```

### Key Architectural Patterns

1. **Tokenization → Parsing → AST**: Source code flows through lexer → parser → AST
2. **Recursive Descent Parsing**: The parser uses recursive descent with precedence climbing
3. **Zero-Copy Where Possible**: Uses `&str` references instead of owned `String`s
4. **Source Location Tracking**: All AST nodes are wrapped in `Spanned<T>` for error reporting
5. **Error Handling**: Custom error types (`LexError`, `ParseError`) with precise location info

### Common Tasks and Locations

#### Adding a New FORTRAN Keyword

1. **Lexer**: Add to `KEYWORDS` constant in `fortran-lexer/src/token.rs`
2. **Token Type**: Add variant to `TokenType` enum in `fortran-lexer/src/token.rs`
3. **Parser**: Add handling in `fortran-parser/src/parser.rs` (usually in `parse_statement_opt()` or similar)
4. **AST**: Add corresponding AST structure if needed (in `fortran-ast/src/statement.rs` or `declaration.rs`)

#### Adding a New Statement Type

1. **AST**: Define in `fortran-ast/src/statement.rs` (add to `Statement` enum)
2. **Parser**: Add parsing method in `fortran-parser/src/parser.rs`:
   - Add detection in `is_executable_statement()` if needed
   - Add case in `parse_statement_opt()`
   - Implement `parse_<statement_type>()` method
3. **Tests**: Add test in `fortran-parser/tests/integration_tests.rs`

#### Adding a New Expression Operator

1. **Lexer**: Add operator to `TokenType` in `fortran-lexer/src/token.rs`
2. **AST**: Add to `BinaryOp` or `UnaryOp` in `fortran-ast/src/expression.rs`
3. **Parser**: 
   - Add to `parse_binary_operator()` or `parse_unary_operator()`
   - Add precedence in `get_operator_precedence()`
   - Handle in expression parsing logic

#### Fixing a Parsing Bug

1. **Identify**: Run `cargo test --package fortran-parser --test integration_tests` to see failing tests
2. **Locate**: Find the relevant parsing method in `fortran-parser/src/parser.rs`
3. **Common Issues**:
   - Token position tracking: `peek()`, `advance()`, `self.current` management
   - Whitespace handling: Use `is_trivial()` to skip whitespace/comments
   - Expression precedence: Check `parse_binary_expression()` and precedence values
   - Statement boundaries: Check `is_executable_statement()` detection logic

### Parser Implementation Details

The parser (`fortran-parser/src/parser.rs`) is a large recursive descent parser:

- **Main entry point**: `parse()` function creates `Parser` and calls `parse_program()`
- **Token management**: 
  - `peek()` - get next non-trivial token (skips whitespace/comments)
  - `advance()` - consume current token and return next non-trivial one
  - `check_token()` - check if current token matches expected type
  - `self.current` - index into `self.tokens` vector
- **Common patterns**:
  - `parse_*_opt()` methods return `Option` (for optional constructs)
  - `parse_*()` methods return `Result` (for required constructs)
  - Use `Spanned::new()` to wrap AST nodes with source location
  - Use `self.create_span()` for span creation

### Error Handling Conventions

- **Lexer errors**: `LexError` with line/column info
- **Parser errors**: `ParseError` enum with variants:
  - `UnexpectedToken { expected, found }`
  - `UnexpectedEof { expected }`
  - `InvalidSyntax { message, line, column }`
- Always include expected tokens and found token in error messages
- Use `self.current_line()` and `self.current_column()` for error locations

### Testing Approach

- **Integration tests**: `fortran-parser/tests/integration_tests.rs` - comprehensive test suite
- **Unit tests**: Inline `#[cfg(test)]` modules in source files
- **Test naming**: `test_parse_<feature>` for parser tests
- **Test structure**: 
  ```rust
  #[test]
  fn test_parse_feature() {
      let source = r#"FORTRAN code here"#;
      let result = parse(source);
      assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
      // ... assertions
  }
  ```

### Adding Fixed-Format FORTRAN Support

**Current Status**: Fixed-format lexer is a TODO in `fortran-lexer/src/lexer.rs:18-22`

**To implement**:
1. Create `FixedFormatLexer` struct (similar to `FreeFormatLexer`)
2. Handle column-based rules:
   - Columns 1-5: Statement label (optional)
   - Column 6: Continuation indicator ('&' or '0')
   - Column 7: Comment indicator ('*' or 'C' or '!')
   - Columns 8-72: Source code
   - Columns 73-80: Ignored (sequence numbers)
3. Handle line continuation (column 6)
4. Update `tokenize()` function to use `FixedFormatLexer` when `Format::FixedFormat`

### Code Style and Conventions

- **Error handling**: Prefer `Result<T, E>` over panics
- **Ownership**: Use references (`&str`) where possible, clone only when necessary
- **Documentation**: Public APIs should have rustdoc comments
- **Naming**: Follow Rust conventions (snake_case for functions, PascalCase for types)
- **Imports**: Group by: std, external crates, workspace crates, local modules

### Debugging Tips

1. **Add debug prints**: `eprintln!("Current token: {:?}", self.peek());`
2. **Check token stream**: `eprintln!("Tokens: {:?}", self.tokens);`
3. **Verify position**: `eprintln!("Current index: {}", self.current);`
4. **Test incrementally**: Run `cargo test --package fortran-parser --test integration_tests <test_name>` for specific tests
5. **Use backtrace**: `RUST_BACKTRACE=1 cargo test ...`

### Quick Reference: File Locations

- **Token definitions**: `fortran-lexer/src/token.rs`
- **Lexer logic**: `fortran-lexer/src/lexer.rs`
- **AST definitions**: `fortran-ast/src/*.rs`
- **Parser logic**: `fortran-parser/src/parser.rs` (main file)
- **Parser tests**: `fortran-parser/tests/integration_tests.rs`
- **Workspace config**: `Cargo.toml` (root)
- **CI/CD**: `.github/workflows/ci.yml`

### When Adding New Features

1. **Start with tests**: Write a failing test first
2. **Update AST**: Add necessary data structures
3. **Update lexer**: Add token types if needed
4. **Update parser**: Implement parsing logic
5. **Run tests**: `cargo test --all`
6. **Update docs**: Add examples and documentation
7. **Run linter**: `cargo clippy --all -- -D warnings`

---

**Built with ❤️ in Rust**

