// Test file for --x-lsp with gopls (Go language server).
//
// The typed metavar pattern (int $X) should match expressions
// whose type is 'int' even when the type can only be determined
// by looking at the definition in another file (lib.go).
//
// Without --x-lsp, semgrep cannot know that Add(x, 1) returns int
// or that ComputeRatio(x, 2) returns float64.
// With --x-lsp, gopls resolves the types across files.
package main

import "fmt"

func main() {
	x := 42
	// Add is defined in lib.go.
	// Only gopls knows Add(x, 1) has type int.
	sum := Add(x, 1)
	// ComputeRatio returns float64, so this should NOT match (int $X).
	ratio := ComputeRatio(x, 2)
	fmt.Printf("sum=%d ratio=%f\n", sum, ratio)
}
