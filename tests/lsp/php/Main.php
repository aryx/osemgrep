<?php
// Test file for --lsp with Intelephense (PHP language server).
//
// The typed metavar pattern ($X: int) should match expressions
// whose type is 'int' even when the type can only be determined
// by looking at the definition in another file (Lib.php).
//
// Without --lsp, semgrep cannot know that Lib::add($a, $b) returns int
// or that Lib::ratio($a, $b) returns float.
// With --lsp, Intelephense resolves the types across files.

require_once 'Lib.php';

$a = 10;
$b = 20;
// Lib::add is defined in Lib.php.
// Only Intelephense knows Lib::add($a, $b) has type int.
$sum = Lib::add($a, $b);
// Lib::ratio returns float, so this should NOT match ($X: int).
$ratio = Lib::ratio($a, $b);
echo "sum=$sum ratio=$ratio\n";
