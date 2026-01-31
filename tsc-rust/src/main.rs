mod lexer;
mod tokens;
mod ast;
mod parser;
mod checker;
mod emitter;
mod compiler;
mod error;

use compiler::TypeScriptCompiler;
use lexer::Lexer;
use serde::Serialize;
use std::collections::{BTreeMap, HashSet};
use std::env;
use std::fs;
use std::time::Instant;
use walkdir::WalkDir;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_usage();
        std::process::exit(1);
    }

    if args[1].starts_with('-') {
        match args[1].as_str() {
            "--help" | "-h" => {
                print_usage();
                return;
            }
            "--version" | "-v" => {
                println!("tsc-rust 2.0.0");
                return;
            }
            "--eval" | "-e" => {
                if args.len() < 3 {
                    eprintln!("Error: --eval requires TypeScript code");
                    std::process::exit(1);
                }
                match TypeScriptCompiler::new().compile(&args[2]) {
                    Ok(js) => println!("{}", js),
                    Err(e) => {
                        eprintln!("Error: {}", e);
                        std::process::exit(1);
                    }
                }
                return;
            }
            "--dump-tokens" => {
                if args.len() < 3 {
                    eprintln!("Error: --dump-tokens requires input file path");
                    std::process::exit(1);
                }
                let source = fs::read_to_string(&args[2]).unwrap_or_else(|e| {
                    eprintln!("Error: Failed to read {}: {}", args[2], e);
                    std::process::exit(1);
                });
                dump_tokens(&source);
                return;
            }
            "--dump-tokens-eval" => {
                if args.len() < 3 {
                    eprintln!("Error: --dump-tokens-eval requires TypeScript code");
                    std::process::exit(1);
                }
                dump_tokens(&args[2]);
                return;
            }
            "--corpus" => {
                if args.len() < 3 {
                    eprintln!("Error: --corpus requires directory path");
                    std::process::exit(1);
                }

                let root = args[2].clone();
                let mut report_path: Option<String> = None;
                let mut limit: Option<usize> = None;
                let mut extensions: Vec<String> = vec!["ts", "tsx"]
                    .into_iter()
                    .map(|s| s.to_string())
                    .collect();

                let mut i = 3;
                while i < args.len() {
                    match args[i].as_str() {
                        "--report" => {
                            if i + 1 >= args.len() {
                                eprintln!("Error: --report requires file path");
                                std::process::exit(1);
                            }
                            report_path = Some(args[i + 1].clone());
                            i += 2;
                        }
                        "--limit" => {
                            if i + 1 >= args.len() {
                                eprintln!("Error: --limit requires a number");
                                std::process::exit(1);
                            }
                            let parsed = args[i + 1].parse::<usize>().unwrap_or_else(|_| {
                                eprintln!("Error: --limit must be a number");
                                std::process::exit(1);
                            });
                            limit = Some(parsed);
                            i += 2;
                        }
                        "--extensions" => {
                            if i + 1 >= args.len() {
                                eprintln!("Error: --extensions requires a comma-separated list");
                                std::process::exit(1);
                            }
                            extensions = args[i + 1]
                                .split(',')
                                .map(|s| s.trim().trim_start_matches('.').to_string())
                                .filter(|s| !s.is_empty())
                                .collect();
                            if extensions.is_empty() {
                                eprintln!("Error: --extensions produced an empty list");
                                std::process::exit(1);
                            }
                            i += 2;
                        }
                        unknown => {
                            eprintln!("Error: Unknown option {}", unknown);
                            std::process::exit(1);
                        }
                    }
                }

                run_ts_corpus(&root, report_path.as_deref(), limit, &extensions);
                return;
            }
            _ => {
                eprintln!("Error: Unknown option {}", args[1]);
                print_usage();
                std::process::exit(1);
            }
        }
    }

    if args.len() < 3 {
        eprintln!("Error: Expected input file and output file");
        print_usage();
        std::process::exit(1);
    }

    let input_path = &args[1];
    let output_path = &args[2];
    let source = fs::read_to_string(input_path).unwrap_or_else(|e| {
        eprintln!("Error: Failed to read {}: {}", input_path, e);
        std::process::exit(1);
    });

    let compiler = TypeScriptCompiler::new();
    let start = Instant::now();
    match compiler.compile(&source) {
        Ok(js) => {
            if let Some(parent) = std::path::Path::new(output_path).parent() {
                if !parent.as_os_str().is_empty() {
                    let _ = fs::create_dir_all(parent);
                }
            }
            fs::write(output_path, js).unwrap_or_else(|e| {
                eprintln!("Error: Failed to write {}: {}", output_path, e);
                std::process::exit(1);
            });
            println!(
                "✓ Compiled {} → {} ({:.2}s)",
                input_path,
                output_path,
                start.elapsed().as_secs_f64()
            );
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    }
}

fn dump_tokens(source: &str) {
    let mut lexer = Lexer::new(source);
    let (tokens, positions) = lexer.tokenize_with_positions().unwrap_or_else(|e| {
        eprintln!("Error: Tokenization failed: {}", e);
        std::process::exit(1);
    });
    for (i, (tok, pos)) in tokens.iter().zip(positions.iter()).take(200).enumerate() {
        println!("{:>4}  {:?}  @{:?}", i, tok, pos);
    }
}

#[derive(Serialize)]
struct CorpusErrorBucket {
    count: usize,
    examples: Vec<String>,
}

#[derive(Serialize)]
struct CorpusReport {
    timestamp_utc: chrono::DateTime<chrono::Utc>,
    root: String,
    extensions: Vec<String>,
    limit: Option<usize>,
    total_discovered: usize,
    attempted: usize,
    compiled: usize,
    failed: usize,
    success_rate: f64,
    failures_by_signature: BTreeMap<String, CorpusErrorBucket>,
    top_signatures: Vec<(String, usize)>,
}

fn run_ts_corpus(root: &str, report_path: Option<&str>, limit: Option<usize>, extensions: &[String]) {
    let ext_set: HashSet<String> = extensions
        .iter()
        .map(|s| s.trim().trim_start_matches('.').to_ascii_lowercase())
        .filter(|s| !s.is_empty())
        .collect();

    if ext_set.is_empty() {
        eprintln!("Error: No extensions provided");
        std::process::exit(1);
    }

    let compiler = TypeScriptCompiler::new();
    let start_all = Instant::now();

    let mut total_discovered = 0usize;
    let mut attempted = 0usize;
    let mut compiled = 0usize;
    let mut failed = 0usize;
    let mut failures_by_signature: BTreeMap<String, CorpusErrorBucket> = BTreeMap::new();

    for entry in WalkDir::new(root)
        .follow_links(false)
        .sort_by_file_name()
        .into_iter()
        .filter_entry(|e| {
            let name = e.file_name().to_string_lossy();
            name != "node_modules" && name != ".git" && name != "target"
        })
    {
        let entry = match entry {
            Ok(e) => e,
            Err(_) => continue,
        };
        if !entry.file_type().is_file() {
            continue;
        }

        let path = entry.path();
        let ext = path
            .extension()
            .and_then(|s| s.to_str())
            .unwrap_or("")
            .to_ascii_lowercase();
        if !ext_set.contains(&ext) {
            continue;
        }

        total_discovered += 1;
        if let Some(max) = limit {
            if attempted >= max {
                break;
            }
        }

        attempted += 1;
        let file_path = path.to_string_lossy().to_string();
        let source = match fs::read_to_string(path) {
            Ok(s) => s,
            Err(e) => {
                failed += 1;
                let signature = format!("ReadError: {}", e);
                let bucket = failures_by_signature
                    .entry(signature)
                    .or_insert(CorpusErrorBucket {
                        count: 0,
                        examples: Vec::new(),
                    });
                bucket.count += 1;
                if bucket.examples.len() < 5 {
                    bucket.examples.push(file_path);
                }
                continue;
            }
        };

        match compiler.compile(&source) {
            Ok(_) => {
                compiled += 1;
            }
            Err(e) => {
                failed += 1;
                let signature = e.to_string();
                let bucket = failures_by_signature
                    .entry(signature)
                    .or_insert(CorpusErrorBucket {
                        count: 0,
                        examples: Vec::new(),
                    });
                bucket.count += 1;
                if bucket.examples.len() < 5 {
                    bucket.examples.push(file_path);
                }
            }
        }
    }

    let success_rate = if attempted == 0 {
        0.0
    } else {
        (compiled as f64) / (attempted as f64) * 100.0
    };

    let mut top: Vec<(String, usize)> = failures_by_signature
        .iter()
        .map(|(sig, bucket)| (sig.clone(), bucket.count))
        .collect();
    top.sort_by(|a, b| b.1.cmp(&a.1));
    top.truncate(20);

    let report = CorpusReport {
        timestamp_utc: chrono::Utc::now(),
        root: root.to_string(),
        extensions: extensions.to_vec(),
        limit,
        total_discovered,
        attempted,
        compiled,
        failed,
        success_rate,
        failures_by_signature,
        top_signatures: top.clone(),
    };

    let json = serde_json::to_string_pretty(&report).unwrap_or_else(|e| {
        eprintln!("Error: Failed to serialize report: {}", e);
        std::process::exit(1);
    });

    let output_path = report_path.map(|s| s.to_string()).unwrap_or_else(|| {
        let ts = chrono::Utc::now().format("%Y%m%d_%H%M%S").to_string();
        format!("corpus_report_{}.json", ts)
    });

    if let Some(parent) = std::path::Path::new(&output_path).parent() {
        if !parent.as_os_str().is_empty() {
            let _ = fs::create_dir_all(parent);
        }
    }

    if let Err(e) = fs::write(&output_path, json) {
        eprintln!("Error: Failed to write report {}: {}", output_path, e);
        std::process::exit(1);
    }

    let elapsed = start_all.elapsed();
    println!("📦 CORPUS COMPLETE");
    println!("   Root: {}", root);
    println!("   Extensions: {}", extensions.join(", "));
    println!("   Total discovered: {}", total_discovered);
    println!("   Attempted: {}", attempted);
    println!("   Compiled: {}", compiled);
    println!("   Failed: {}", failed);
    println!("   Success rate: {:.2}%", success_rate);
    println!("   Elapsed: {:.2}s", elapsed.as_secs_f64());
    println!("   Report: {}", output_path);
    println!();
    println!("🔥 TOP FAILURE SIGNATURES");
    for (sig, count) in top {
        println!("   {:>5}  {}", count, sig);
    }
}

fn print_usage() {
    eprintln!("Usage:");
    eprintln!("  tsc-rust <input.ts> <output.js>");
    eprintln!("  tsc-rust --eval \"export const x = 1\"");
    eprintln!("  tsc-rust --dump-tokens <input.ts>");
    eprintln!("  tsc-rust --dump-tokens-eval \"export type {{ A }};\"");
    eprintln!("  tsc-rust --corpus <directory> [--report <file>] [--limit <n>] [--extensions ts,tsx]");
}
