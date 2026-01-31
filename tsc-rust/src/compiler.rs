//! TypeScript Compiler - Main compilation pipeline

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::checker::TypeChecker;
use crate::emitter::Emitter;
use crate::error::CompileError;
use crate::ast::Program;

pub struct TypeScriptCompiler;

/// Compilation result with both JavaScript and AST
#[derive(Debug, Clone)]
pub struct CompilationResult {
    pub js: String,
    pub ast: Program,
}

impl TypeScriptCompiler {
    pub fn new() -> Self {
        Self
    }
    
    /// Compile TypeScript source to JavaScript
    pub fn compile(&self, source: &str) -> Result<String, CompileError> {
        let result = self.compile_with_ast(source)?;
        Ok(result.js)
    }
    
    /// Compile TypeScript source to JavaScript and AST
    /// Used by SIR bridge for Phase 7 integration
    pub fn compile_with_ast(&self, source: &str) -> Result<CompilationResult, CompileError> {
        // 1. LEXER: Source → Tokens
        let mut lexer = Lexer::new(source);
        let (tokens, positions) = lexer.tokenize_with_positions()?;
        
        // 2. PARSER: Tokens → AST
        let mut parser = Parser::new_with_positions(tokens, positions);
        let ast = parser.parse()?;
        
        // 3. TYPE CHECKER: Validate types
        let mut checker = TypeChecker::new();
        checker.check(&ast)?;
        
        // 4. EMITTER: AST → JavaScript
        let mut emitter = Emitter::new();
        let js_code = emitter.emit(&ast);
        
        Ok(CompilationResult { js: js_code, ast })
    }
    
    /// Compile without type checking (faster)
    /// Used by Tauri UI for quick compilation
    #[allow(dead_code)] // Used in Tauri UI
    pub fn compile_fast(&self, source: &str) -> Result<String, CompileError> {
        let mut lexer = Lexer::new(source);
        let (tokens, positions) = lexer.tokenize_with_positions()?;
        
        let mut parser = Parser::new_with_positions(tokens, positions);
        let ast = parser.parse()?;
        
        let mut emitter = Emitter::new();
        let js_code = emitter.emit(&ast);
        
        Ok(js_code)
    }
}
