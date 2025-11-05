/// FORTRAN literal values.
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// Integer literal
    Integer(String),
    
    /// Real (floating-point) literal
    Real(String),
    
    /// Complex literal (real, imag)
    Complex {
        real: String,
        imag: String,
    },
    
    /// Character literal (string)
    Character(String),
    
    /// Logical literal (.TRUE. or .FALSE.)
    Logical(bool),
}

impl Literal {
    pub fn integer(value: &str) -> Self {
        Self::Integer(value.to_string())
    }
    
    pub fn real(value: &str) -> Self {
        Self::Real(value.to_string())
    }
    
    pub fn complex(real: &str, imag: &str) -> Self {
        Self::Complex {
            real: real.to_string(),
            imag: imag.to_string(),
        }
    }
    
    pub fn character(value: &str) -> Self {
        Self::Character(value.to_string())
    }
    
    pub fn logical(value: bool) -> Self {
        Self::Logical(value)
    }
}

