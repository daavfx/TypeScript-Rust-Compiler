//! Compiler error types

use std::fmt;

#[derive(Debug, Clone)]
pub struct CompileError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl CompileError {
    pub fn new(message: String, line: usize, col: usize) -> Self {
        Self { message, line, col }
    }
    
    pub fn simple(message: &str) -> Self {
        Self {
            message: message.to_string(),
            line: 0,
            col: 0,
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.line > 0 {
            write!(f, "Error at {}:{}: {}", self.line, self.col, self.message)
        } else {
            write!(f, "Error: {}", self.message)
        }
    }
}

impl std::error::Error for CompileError {}