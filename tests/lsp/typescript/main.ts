// Test file for --lsp with typescript-language-server.
//
// The typed metavar pattern ($X : number) should match expressions
// whose type is 'number', even when the type can only be determined
// by looking at the definition in another file (lib.ts).
//
// Without --lsp, semgrep cannot know that add(x, 1) returns number
// or that concat("a", "b") returns string.
// With --lsp, typescript-language-server resolves the types across files.

import { add, concat } from "./lib";

const x: number = 42;
// add is defined in lib.ts.
// Only the type checker knows add(x, 1) has type number.
const result_num = add(x, 1);
// concat returns string, so this should NOT match ($X : number).
const result_str = concat("hello", " world");
console.log(x, result_num, result_str);
