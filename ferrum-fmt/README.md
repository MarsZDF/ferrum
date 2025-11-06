# ferrum-fmt

Auto-formatter for FORTRAN source code (like `rustfmt` or `black`).

## Features

- ✅ Configurable indentation (spaces/tabs, width)
- ✅ Keyword case normalization (UPPER, lower, preserve)
- ✅ Identifier case normalization
- ✅ Spacing around operators
- ✅ Column alignment for declarations
- ✅ Line length enforcement
- ✅ Comment preservation
- ✅ Line continuation handling

## Installation

```bash
cargo install --path ferrum-fmt
```

Or use it as a library in your Rust project:

```toml
[dependencies]
ferrum-fmt = { path = "../ferrum-fmt" }
```

## Usage

### Command Line

Format a file:

```bash
ferrum-fmt input.f90
```

Format and write back to file:

```bash
ferrum-fmt --write input.f90
```

Check if files are formatted (useful for CI):

```bash
ferrum-fmt --check *.f90
```

Format with FORTRAN 77 style:

```bash
ferrum-fmt --fortran77 input.f
```

Format with modern style:

```bash
ferrum-fmt --modern input.f90
```

Custom options:

```bash
ferrum-fmt --indent-width 4 --keyword-case upper --max-line-length 120 input.f90
```

### Library Usage

```rust
use ferrum_fmt::{format_source, FormatConfig};
use fortran_lexer::detect_format;

let source = r#"
program hello
    print *, 'Hello, World!'
end program hello
"#;

let format = detect_format(source);
let config = FormatConfig::modern();
let formatted = format_source(source, format, config)?;
println!("{}", formatted);
```

## Configuration

### FormatConfig Options

- `indent_width`: Number of spaces per indentation level (default: 2)
- `use_spaces`: Use spaces instead of tabs (default: true)
- `keyword_case`: Keyword case convention - `Upper`, `Lower`, or `Preserve` (default: `Preserve`)
- `identifier_case`: Identifier case convention - `Upper`, `Lower`, or `Preserve` (default: `Preserve`)
- `space_around_operators`: Add spaces around operators (default: true)
- `align_declarations`: Align declarations in columns (default: true)
- `max_line_length`: Maximum line length before wrapping (default: 132)
- `preserve_continuations`: Preserve existing line continuations (default: true)

### Preset Styles

**FORTRAN 77 Style:**
- 6-space indent
- Uppercase keywords
- 72-character line length

**Modern Style:**
- 2-space indent
- Lowercase keywords and identifiers
- 132-character line length

## Examples

See the `examples/` directory for more usage examples.

## Limitations

- Fixed-format FORTRAN support is limited (free-format is fully supported)
- Some complex formatting scenarios may require manual adjustment
- Comment preservation is best-effort

## Contributing

Contributions are welcome! Please see the main Ferrum repository for contribution guidelines.

## License

MIT License - see LICENSE file for details.

