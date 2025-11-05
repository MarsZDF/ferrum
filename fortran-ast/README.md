# fortran-ast

Abstract Syntax Tree (AST) data structures for FORTRAN programs.

## Features

- ✅ Complete AST representation of FORTRAN program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
- ✅ Declaration structures (variable declarations, type specifications, attributes)
- ✅ Expression trees (arithmetic, logical, comparison, function calls)
- ✅ Statement structures (IF, DO, SELECT CASE, I/O statements, etc.)
- ✅ Source span tracking for all nodes
- ✅ Visitor pattern for AST traversal
- ✅ Optional serialization support (serde)

## Usage

```rust
use fortran_ast::{Program, Expression, Statement, TypeSpec};

// Build AST nodes programmatically
let expr = Expression::Literal(Literal::integer("42"));
// ... use in AST
```

## Structure

The AST is organized into several modules:

- **`program`**: Top-level program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
- **`declaration`**: Variable declarations, type specifications, attributes
- **`statement`**: Executable statements (IF, DO, READ, WRITE, etc.)
- **`expression`**: Expressions (literals, variables, operators, function calls)
- **`literal`**: Literal values (integer, real, complex, character, logical)
- **`span`**: Source location tracking
- **`visitor`**: Visitor pattern for AST traversal

## License

MIT

