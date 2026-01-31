# tsc-rust

TypeScript/JavaScript compiler written in Rust.

## Build

```bash
cargo build --bin tsc-rust
```

The binary is at `target/debug/tsc-rust.exe` (Windows).

## Use

Compile a file:

```bash
tsc-rust <input.ts> <output.js>
```

Compile an expression/snippet:

```bash
tsc-rust --eval "const a = 0x89;"
```

Run a corpus (compile many files and produce a JSON report):

```bash
tsc-rust --corpus <dir> --extensions ts,tsx --limit 50 --report corpus_report.json
```

## Notes

- `god_mode` is an optional feature flag intended for experimental/dev-only behavior.
