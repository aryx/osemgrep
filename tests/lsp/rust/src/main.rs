// Test file for --lsp with rust-analyzer.
//
// The typed metavar pattern ($X: i32) should match expressions
// whose type is 'i32' even when the type can only be determined
// by looking at the definition in another file (lib.rs).
//
// Without --lsp, semgrep cannot know that add(x, 1) returns i32
// or that compute_ratio(x, 2) returns f64.
// With --lsp, rust-analyzer resolves the types across files.

use lsptest::{add, compute_ratio};

fn main() {
    let x: i32 = 42;
    // add is defined in lib.rs.
    // Only rust-analyzer knows add(x, 1) has type i32.
    let sum = add(x, 1);
    // compute_ratio returns f64, so this should NOT match ($X: i32).
    let ratio = compute_ratio(x, 2);
    println!("sum={} ratio={}", sum, ratio);
}
