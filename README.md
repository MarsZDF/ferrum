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

**Status**: 🚧 In Development

[📖 Documentation](fortran-lexer/README.md)

### fortran-ast
Abstract Syntax Tree (AST) data structures for FORTRAN programs.

**Status**: 🚧 Planned

[📖 Documentation](fortran-ast/README.md)

### fortran-parser
Recursive descent parser that converts tokens into a structured AST.

**Status**: 🚧 Planned

[📖 Documentation](fortran-parser/README.md)

## 🚀 Quick Start

### Installation

```bash
# Clone the repository
git clone https://github.com/MarsZDF/ferrum.git
cd ferrum

# Build all crates
cargo build --all
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
```

## 📝 Contributing

Contributions are welcome! This project follows standard Rust conventions:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Ensure all tests pass
6. Submit a pull request

## 📄 License

Licensed under the MIT License - see [LICENSE](LICENSE) for details.

## 🗺️ Roadmap

### In Progress 🚧
- [ ] fortran-lexer - Free-format FORTRAN lexer
- [ ] fortran-lexer - Fixed-format FORTRAN lexer

### Planned 📋
- [ ] fortran-ast - Core AST structures
- [ ] fortran-parser - Basic parser implementation
- [ ] fortran-analyzer-* - Analysis modules
- [ ] Language server support (LSP)
- [ ] Formatter
- [ ] Refactoring tools

## 🤝 Acknowledgments

This project aims to modernize FORTRAN tooling using Rust's excellent performance and safety guarantees.

---

**Built with ❤️ in Rust**

