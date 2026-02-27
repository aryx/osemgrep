# Test file for --lsp with ty / pyright (Python language servers).
#
# The typed metavar pattern show($X: int) should match calls to show()
# where the argument's type is 'int', even when the type can only be
# determined by looking at the definition in another file (lib.py).
#
# Without --lsp, semgrep cannot know that add(x, 1) returns int
# or that compute_ratio(x, 2) returns float.
# With --lsp, ty/pyright resolves the types across files.

from lib import add, compute_ratio, show

x: int = 42
# add is defined in lib.py.
# Only the type checker knows add(x, 1) has type int.
result_int = add(x, 1)
# compute_ratio returns float, so this should NOT match show($X: int).
result_float = compute_ratio(x, 2)
# show() calls — the pattern show($X: int) should match some of these.
show(x)
show(result_int)
show(result_float)
show(42)
