# TypeScript-Rust-Compiler

A TypeScript-to-JavaScript compiler written in Rust.

**Author:** Ernesto (daavfx)  
**Assistance:** AI (mainly chinese free models, deepseek api my GOAT and trae.ai - NOT a single line of code using ZED)  
**Status:** Compiles 3,145 real-world TypeScript files successfully

---

## Verified Results

```bash
$ tsc-rust --corpus openclaw-main

📦 CORPUS COMPLETE
   Total discovered: 3145
   Compiled: 3145
   Failed: 0
   Success rate: 100.00%
   Elapsed: 12.93s

What It Does
Compiles TypeScript (.ts/.tsx) to JavaScript (.js) with type checking:

Classes, interfaces, generics
Async/await, decorators
JSX/TSX components
ESM and CommonJS modules
37+ keywords as property names

Build
cd tsc-rust
cargo build --release
./target/release/tsc-rust input.ts output.js
Context
Microsoft is building an official TypeScript compiler in Go (announcement). This project explores "what if we did it in Rust?" It works for real code but lacks LSP support and IDE integration that Microsoft's solution will have.

License
Copyright (c) 2026 Ernesto (daavfx) - See LICENSE
