// Prevents additional console window on Windows in release builds
#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use tsc_rust::TypeScriptCompiler;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
struct CompileRequest {
    source: String,
    skip_type_check: bool,
}

#[derive(Debug, Serialize, Deserialize)]
struct CompileResponse {
    success: bool,
    output: Option<String>,
    error: Option<String>,
}

/// Tauri command: Compile TypeScript to JavaScript
#[tauri::command]
fn compile_typescript(request: CompileRequest) -> CompileResponse {
    let compiler = TypeScriptCompiler::new();
    
    let result = if request.skip_type_check {
        compiler.compile_fast(&request.source)
    } else {
        compiler.compile(&request.source)
    };
    
    match result {
        Ok(js_code) => CompileResponse {
            success: true,
            output: Some(js_code),
            error: None,
        },
        Err(e) => CompileResponse {
            success: false,
            output: None,
            error: Some(format!("{}", e)),
        },
    }
}

fn main() {
    tauri::Builder::default()
        .invoke_handler(tauri::generate_handler![compile_typescript])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}

