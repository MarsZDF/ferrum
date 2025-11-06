use std::collections::HashMap;
use crate::span::Span;

/// Symbol table for tracking variable and subroutine definitions and usage
#[derive(Debug, Clone)]
pub struct SymbolTable {
    /// Stack of scopes (global, subroutine-local, etc.)
    scopes: Vec<Scope>,
    /// Index of current scope
    current_scope: usize,
}

/// A scope containing symbol definitions
#[derive(Debug, Clone)]
pub struct Scope {
    /// Symbols defined in this scope
    symbols: HashMap<String, Symbol>,
    /// Parent scope index
    parent: Option<usize>,
    /// Scope type (global, subroutine, etc.)
    scope_type: ScopeType,
}

/// Types of scopes in FORTRAN
#[derive(Debug, Clone, PartialEq)]
pub enum ScopeType {
    Global,
    Program,
    Subroutine(String),
    Function(String),
    Module(String),
    Block,
}

/// Information about a symbol (variable, subroutine, function)
#[derive(Debug, Clone)]
pub struct Symbol {
    /// Symbol name
    pub name: String,
    /// Type of symbol
    pub symbol_type: SymbolType,
    /// Where it was defined
    pub defined_at: Span,
    /// All locations where it's used
    pub used_at: Vec<Span>,
    /// Additional metadata
    pub metadata: SymbolMetadata,
}

/// Types of symbols in FORTRAN
#[derive(Debug, Clone, PartialEq)]
pub enum SymbolType {
    Variable {
        data_type: String,
        dimensions: Vec<String>,
    },
    Subroutine {
        parameters: Vec<String>,
    },
    Function {
        parameters: Vec<String>,
        return_type: String,
    },
    Parameter,
    Label(u32),
    Module,
}

/// Additional metadata for symbols
#[derive(Debug, Clone, Default)]
pub struct SymbolMetadata {
    /// Whether symbol is actually used (for dead code detection)
    pub is_used: bool,
    /// Whether symbol is exported from module
    pub is_public: bool,
    /// Whether symbol is imported
    pub is_imported: bool,
    /// Call graph information for subroutines/functions
    pub calls: Vec<String>,
    /// Called by information
    pub called_by: Vec<String>,
}

impl SymbolTable {
    /// Create a new symbol table with global scope
    pub fn new() -> Self {
        let global_scope = Scope {
            symbols: HashMap::new(),
            parent: None,
            scope_type: ScopeType::Global,
        };
        
        Self {
            scopes: vec![global_scope],
            current_scope: 0,
        }
    }
    
    /// Enter a new scope
    pub fn enter_scope(&mut self, scope_type: ScopeType) {
        let new_scope = Scope {
            symbols: HashMap::new(),
            parent: Some(self.current_scope),
            scope_type,
        };
        
        self.scopes.push(new_scope);
        self.current_scope = self.scopes.len() - 1;
    }
    
    /// Exit current scope
    pub fn exit_scope(&mut self) {
        if let Some(parent) = self.scopes[self.current_scope].parent {
            self.current_scope = parent;
        }
    }
    
    /// Define a new symbol in current scope
    pub fn define_symbol(&mut self, symbol: Symbol) {
        self.scopes[self.current_scope]
            .symbols
            .insert(symbol.name.clone(), symbol);
    }
    
    /// Look up a symbol (searches up scope chain)
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        let mut current = self.current_scope;
        
        loop {
            if let Some(symbol) = self.scopes[current].symbols.get(name) {
                return Some(symbol);
            }
            
            if let Some(parent) = self.scopes[current].parent {
                current = parent;
            } else {
                break;
            }
        }
        
        None
    }
    
    /// Mark a symbol as used
    pub fn mark_used(&mut self, name: &str, location: Span) {
        if let Some(symbol) = self.lookup_mut(name) {
            symbol.used_at.push(location);
            symbol.metadata.is_used = true;
        }
    }
    
    /// Get mutable reference to symbol
    fn lookup_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        let mut current = self.current_scope;
        
        loop {
            if self.scopes[current].symbols.contains_key(name) {
                return self.scopes[current].symbols.get_mut(name);
            }
            
            if let Some(parent) = self.scopes[current].parent {
                current = parent;
            } else {
                break;
            }
        }
        
        None
    }
    
    /// Get all symbols in all scopes
    pub fn all_symbols(&self) -> Vec<&Symbol> {
        self.scopes
            .iter()
            .flat_map(|scope| scope.symbols.values())
            .collect()
    }
    
    /// Get unused symbols (dead code candidates)
    pub fn unused_symbols(&self) -> Vec<&Symbol> {
        self.all_symbols()
            .into_iter()
            .filter(|symbol| !symbol.metadata.is_used)
            .collect()
    }
    
    /// Get all subroutines/functions
    pub fn get_callable_symbols(&self) -> Vec<&Symbol> {
        self.all_symbols()
            .into_iter()
            .filter(|symbol| matches!(
                symbol.symbol_type,
                SymbolType::Subroutine { .. } | SymbolType::Function { .. }
            ))
            .collect()
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Call graph for tracking subroutine/function dependencies
#[derive(Debug, Clone)]
pub struct CallGraph {
    /// Map from caller to list of callees
    calls: HashMap<String, Vec<String>>,
    /// Map from callee to list of callers  
    called_by: HashMap<String, Vec<String>>,
    /// All known subroutines/functions
    procedures: HashMap<String, ProcedureInfo>,
}

/// Information about a procedure (subroutine or function)
#[derive(Debug, Clone)]
pub struct ProcedureInfo {
    pub name: String,
    pub defined_at: Span,
    pub parameters: Vec<String>,
    pub is_main: bool,
    pub is_external: bool,
}

impl CallGraph {
    pub fn new() -> Self {
        Self {
            calls: HashMap::new(),
            called_by: HashMap::new(),
            procedures: HashMap::new(),
        }
    }
    
    /// Add a procedure definition
    pub fn add_procedure(&mut self, info: ProcedureInfo) {
        self.procedures.insert(info.name.clone(), info);
    }
    
    /// Record a call from caller to callee
    pub fn add_call(&mut self, caller: &str, callee: &str) {
        self.calls
            .entry(caller.to_string())
            .or_default()
            .push(callee.to_string());
            
        self.called_by
            .entry(callee.to_string())
            .or_default()
            .push(caller.to_string());
    }
    
    /// Get all procedures called by a given procedure
    pub fn get_calls(&self, procedure: &str) -> Option<&Vec<String>> {
        self.calls.get(procedure)
    }
    
    /// Get all procedures that call a given procedure
    pub fn get_callers(&self, procedure: &str) -> Option<&Vec<String>> {
        self.called_by.get(procedure)
    }
    
    /// Find dead procedures (not called by anything, not main)
    pub fn find_dead_procedures(&self) -> Vec<&ProcedureInfo> {
        self.procedures
            .values()
            .filter(|proc| {
                !proc.is_main && 
                !proc.is_external &&
                !self.called_by.contains_key(&proc.name)
            })
            .collect()
    }
    
    /// Find all procedures reachable from main/public entry points
    pub fn find_reachable_procedures(&self) -> Vec<String> {
        let mut reachable = Vec::new();
        let mut to_visit = Vec::new();
        
        // Start with main programs and external procedures
        for proc in self.procedures.values() {
            if proc.is_main || proc.is_external {
                to_visit.push(proc.name.clone());
            }
        }
        
        while let Some(current) = to_visit.pop() {
            if reachable.contains(&current) {
                continue;
            }
            
            reachable.push(current.clone());
            
            // Add all procedures called by current
            if let Some(calls) = self.get_calls(&current) {
                for called in calls {
                    if !reachable.contains(called) {
                        to_visit.push(called.clone());
                    }
                }
            }
        }
        
        reachable
    }
    
    /// Get all procedure names
    pub fn all_procedure_names(&self) -> Vec<String> {
        self.procedures.keys().cloned().collect()
    }
    
    /// Get procedure info by name
    pub fn get_procedure(&self, name: &str) -> Option<&ProcedureInfo> {
        self.procedures.get(name)
    }
}

impl Default for CallGraph {
    fn default() -> Self {
        Self::new()
    }
}