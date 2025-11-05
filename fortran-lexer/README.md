# fortran-lexer

Fast, modular lexer for FORTRAN source code supporting both fixed-format and free-format FORTRAN.

## Features

- ✅ Free-format FORTRAN lexing (FORTRAN 90+)
- ✅ Case-insensitive keyword recognition
- ✅ Comprehensive token types (keywords, identifiers, literals, operators, punctuation)
- ✅ Source location tracking (line, column, span)
- ✅ Error reporting with precise location information
- 🚧 Fixed-format FORTRAN lexing (in progress)

## Usage

```rust
use fortran_lexer::{tokenize, Format};

let source = r#"
program hello
    implicit none
    integer :: i
    print *, 'Hello, World!'
end program hello
"#;

let tokens = tokenize(source, Format::FreeFormat)?;
for token in tokens {
    if !token.is_trivial() {
        println!("{:?}", token.token_type);
    }
}
```

## Token Types

The lexer recognizes:
- **Keywords**: PROGRAM, SUBROUTINE, FUNCTION, IF, THEN, ELSE, DO, etc.
- **Data types**: INTEGER, REAL, COMPLEX, CHARACTER, LOGICAL, etc.
- **Operators**: Arithmetic (+, -, *, /, **), Comparison (=, /=, <, <=, >, >=), Logical (.AND., .OR., .NOT.)
- **Literals**: Integer, Real, Complex, Character, Logical (.TRUE., .FALSE.)
- **Punctuation**: Parentheses, commas, colons, semicolons, etc.

## Examples

See [examples/basic_tokenize.rs](examples/basic_tokenize.rs) for a complete example.

```bash
cargo run --example basic_tokenize
```

## License

MIT

