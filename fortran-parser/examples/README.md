# FORTRAN Parser Examples

This directory contains practical examples demonstrating the capabilities of the Ferrum FORTRAN parser, including tools for analyzing and modernizing legacy FORTRAN code.

## Available Examples

### Core Parsing Examples

#### `basic_parse.rs`
Basic example demonstrating how to parse FORTRAN source code and inspect the resulting AST.

```bash
cargo run --example basic_parse
```

#### `error_handling.rs` 
Shows how to handle parsing errors gracefully and extract useful error information.

```bash
cargo run --example error_handling
```

#### `scientific_computing.rs`
Example parsing scientific computing FORTRAN code with numerical algorithms.

```bash
cargo run --example scientific_computing
```

### Modernization Analysis Tools

These tools help analyze legacy FORTRAN code and plan modernization efforts:

#### `extract_signature.rs` - Interface Extraction
Extracts FORTRAN subroutine signatures and generates modern Rust/Python equivalents.

**Usage:**
```bash
cargo run --example extract_signature <fortran-file>
```

**Example output:**
```rust
// Original FORTRAN 77 signature:
//   SUBROUTINE COMPUTE(MODE, M, N, Y, F, MATRIX, LDM)

// Modern Rust equivalent:
pub fn compute(
    mode: &mut i32,
    m: usize,
    n: usize,
    y: &[f64],
    f: &[f64],
    matrix: &mut [f64],
    ldm: usize,
) -> Result<(), ComputeError> {
    // Implementation here
    Ok(())
}

// Modern Python equivalent:
def compute(
    mode: int,
    m: int,
    n: int,
    y: np.ndarray,
    f: np.ndarray,
    matrix: np.ndarray,
    ldm: int
) -> ComputeResult:
    """Generated from FORTRAN subroutine."""
    # Implementation here
    pass
```

#### `extract_docs.rs` - Documentation Extraction
Extracts and formats documentation from FORTRAN comment blocks.

**Usage:**
```bash
cargo run --example extract_docs <fortran-file>
```

**Features:**
- Extracts parameter descriptions
- Identifies algorithm descriptions
- Generates modern documentation format
- Preserves original comment structure

#### `type_mapper.rs` - Type Analysis
Analyzes FORTRAN types and generates modern type mappings for migration planning.

**Usage:**
```bash
cargo run --example type_mapper <fortran-file>
```

**Output:**
- Parameter intent analysis (input/output/inout)
- Modern type equivalents (Rust and Python)
- Generated interface structures
- Migration recommendations

#### `fixed_to_free.rs` - Format Converter
Converts fixed-format FORTRAN 77 to modern free-format FORTRAN.

**Usage:**
```bash
# Convert and save to file
cargo run --example fixed_to_free legacy.f modern.f90

# Convert and output to stdout
cargo run --example fixed_to_free legacy.f
```

**Features:**
- Converts column-based fixed-format to free-format
- Modernizes comment style (c/C/* → !)
- Preserves statement labels and logic flow
- Handles line continuation properly
- Applies appropriate indentation

## Format Support

All examples work with both:
- **Free-format FORTRAN** (FORTRAN 90+)
- **Fixed-format FORTRAN** (FORTRAN 77 and earlier)

The parser automatically detects the format and uses the appropriate lexer.

### Fixed-Format Example
```fortran
c This is a FORTRAN 77 comment
      SUBROUTINE HELLO(N, X, Y)
      INTEGER N
      REAL X(N), Y(N)
      DO 10 I = 1, N
         Y(I) = X(I) * 2.0
10    CONTINUE
      END
```

### Free-Format Example
```fortran
! This is a modern FORTRAN comment
subroutine hello(n, x, y)
    implicit none
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    real, intent(out) :: y(n)
    integer :: i
    
    do i = 1, n
        y(i) = x(i) * 2.0
    end do
end subroutine hello
```

## Real-World Applications

These tools are designed for practical legacy code modernization:

### Government & Defense
- Analyzing decades-old simulation code
- Planning migration to modern languages
- Extracting API documentation from undocumented code

### Scientific Computing
- Modernizing numerical libraries (LAPACK, BLAS, etc.)
- Converting climate models to cloud-friendly formats
- Preserving computational physics heritage

### Financial Systems
- Updating legacy trading algorithms
- Risk model modernization
- Regulatory compliance documentation

### Academic Research
- Digital preservation of historical code
- Computer science education
- Programming language research

## Development

### Adding New Examples

1. Create a new `.rs` file in this directory
2. Follow the existing pattern:
   ```rust
   use std::env;
   use std::fs;
   use fortran_parser::parse;
   
   fn main() -> Result<(), Box<dyn std::error::Error>> {
       let args: Vec<String> = env::args().collect();
       if args.len() < 2 {
           eprintln!("Usage: {} <fortran-file>", args[0]);
           std::process::exit(1);
       }
       
       let source = fs::read_to_string(&args[1])?;
       let ast = parse(&source)?;
       
       // Your analysis here
       
       Ok(())
   }
   ```
3. Add documentation to this README

### Testing Examples

```bash
# Test all examples compile
cargo build --examples

# Run specific example
cargo run --example <example_name> <test_file>
```

## Contributing

When adding examples:
- Include comprehensive error handling
- Provide clear usage instructions
- Add practical output examples
- Test with both fixed and free format FORTRAN
- Document real-world use cases