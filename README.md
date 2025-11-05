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
- [ ] fortran-parser - Full FORTRAN grammar support
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

**Built with ❤️ in Rust**

