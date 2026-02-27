/* Test file for --lsp with clangd.
 *
 * The typed metavar pattern (int $X) should match expressions
 * whose type is 'int' even when the type can only be determined
 * by looking at the declaration in another file (lib.h / lib.c).
 *
 * Without --lsp, semgrep cannot know that add(x, 1) returns int
 * or that compute_ratio(x, 2) returns float.
 * With --lsp, clangd resolves the types across files.
 */
#include "lib.h"

int main(void) {
    int x = 42;
    /* add() is defined in lib.c, declared in lib.h.
     * Only clangd (via the header) knows add(x, 1) has type int. */
    int sum = add(x, 1);
    /* compute_ratio() returns float, so this should NOT match (int $X). */
    float ratio = compute_ratio(x, 2);
    return sum;
}
