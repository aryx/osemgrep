// Test file for --lsp with jdtls (Java language server).
//
// The typed metavar pattern (int $X) should match expressions
// whose type is 'int' even when the type can only be determined
// by looking at the definition in another file (Lib.java).
//
// Without --lsp, semgrep cannot know that Lib.add(a, b) returns int
// or that Lib.ratio(a, b) returns double.
// With --lsp, jdtls resolves the types across files.

public class Main {
    public static void main(String[] args) {
        int a = 10;
        int b = 20;
        // Lib.add is defined in Lib.java.
        // Only jdtls knows Lib.add(a, b) has type int.
        int sum = Lib.add(a, b);
        // Lib.ratio returns double, so this should NOT match (int $X).
        double ratio = Lib.ratio(a, b);
        System.out.println("sum=" + sum + " ratio=" + ratio);
    }
}
