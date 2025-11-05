# fortran-parser

Recursive descent parser for FORTRAN source code that converts tokens into an Abstract Syntax Tree (AST).

## Features

- ✅ Parses FORTRAN program units (PROGRAM, SUBROUTINE, FUNCTION, MODULE)
- ✅ Parses declarations (variable declarations, type specifications, attributes)
- ✅ Parses executable statements (IF, DO, READ, WRITE, PRINT, etc.)
- ✅ Parses expressions (arithmetic, logical, comparison, function calls)
- ✅ Error reporting with location information
- ✅ Handles whitespace and comments gracefully

## Usage

```rust
use fortran_parser::parse;

let source = r#"
program hello
    implicit none
    integer :: i
    print *, 'Hello, World!'
end program hello
"#;

let program = parse(source)?;
println!("Parsed: {:?}", program);
```

## Examples

See [examples/basic_parse.rs](examples/basic_parse.rs) for a complete example.

```bash
cargo run --example basic_parse
```

## Status

Basic parsing implemented. More FORTRAN constructs are being added.

## License

MIT

