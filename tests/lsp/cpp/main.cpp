// Test file for --lsp with clangd (C++ language server).
//
// The typed metavar pattern (int $X) should match expressions whose type
// is 'int' even when the type can only be determined by looking at the
// class definition in another file (math_utils.h / math_utils.cpp).
//
// Without --lsp, semgrep cannot know that MathUtils::add(x, 1) returns
// int or that identity<int>(x) returns int.
// With --lsp, clangd resolves types across files and through templates.

#include "math_utils.h"

int main() {
    int x = 42;

    // MathUtils::add is declared in math_utils.h, defined in math_utils.cpp.
    // Only clangd knows add() returns int.
    auto sum = MathUtils::add(x, 1);

    // MathUtils::ratio returns double, so this should NOT match (int $X).
    auto r = MathUtils::ratio(x, 2);

    // Template instantiation: identity<int>(x) returns int.
    // Only clangd can resolve the template parameter.
    auto same = identity<int>(x);

    return sum + same;
}
