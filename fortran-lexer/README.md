# fortran-lexer

Fast, modular lexer for FORTRAN source code supporting both fixed-format and free-format FORTRAN.

## Features

- ✅ **Free-format FORTRAN lexing** (FORTRAN 90+)
- ✅ **Fixed-format FORTRAN lexing** (FORTRAN 77 and earlier)
- ✅ **Automatic format detection** (fixed vs free format)
- ✅ **Column-based parsing** for fixed-format (labels, continuation, code sections)
- ✅ **Comment line handling** with any characters (c, C, *, !)
- ✅ **Case-insensitive keyword recognition**
- ✅ **Comprehensive token types** (keywords, identifiers, literals, operators, punctuation)
- ✅ **Source location tracking** (line, column, span)
- ✅ **Error reporting** with precise location information
- ✅ **Real-world compatibility** with legacy numerical libraries

## Usage

### Automatic Format Detection

```rust
use fortran_lexer::{tokenize, detect_format};

// The lexer automatically detects fixed vs free format
let source = r#"
program hello
    implicit none
    integer :: i
    print *, 'Hello, World!'
end program hello
"#;

let format = detect_format(source);
let tokens = tokenize(source, format)?;
for token in tokens {
    if !token.is_trivial() {
        println!("{:?}", token.token_type);
    }
}
```

### Fixed-Format FORTRAN Support

The lexer fully supports legacy FORTRAN 77 fixed-format:

```rust
use fortran_lexer::{tokenize, detect_format};

let legacy_source = r#"
c This is a comment line
      SUBROUTINE COMPUTE(N, X, Y)
      INTEGER N
      REAL X(N), Y(N)
c     Another comment
      DO 10 I = 1, N
         Y(I) = X(I) * 2.0
10    CONTINUE
      END
"#;

let format = detect_format(legacy_source); // Returns FixedFormat
let tokens = tokenize(legacy_source, format)?;
```

**Fixed-format features:**
- **Column-based parsing**: Columns 1-5 (labels), 6 (continuation), 7-72 (code)
- **Comment detection**: Lines starting with `c`, `C`, `*`, or `!`
- **Statement labels**: Numeric labels in columns 1-5
- **Continuation lines**: Non-space/non-zero in column 6
- **Special character support**: Handles all legacy characters including `?`, `"`, etc.

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

