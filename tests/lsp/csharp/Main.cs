// Test file for --lsp with OmniSharp (C# language server).
//
// The typed metavar pattern (int $X) should match expressions
// whose type is 'int' even when the type can only be determined
// by looking at the definition in another file (Lib.cs).
//
// Without --lsp, semgrep cannot know that Lib.Add(a, b) returns int
// or that Lib.Ratio(a, b) returns double.
// With --lsp, OmniSharp resolves the types across files.

public class Program {
    public static void Main(string[] args) {
        int a = 10;
        int b = 20;
        // Lib.Add is defined in Lib.cs.
        // Only OmniSharp knows Lib.Add(a, b) has type int.
        int sum = Lib.Add(a, b);
        // Lib.Ratio returns double, so this should NOT match (int $X).
        double ratio = Lib.Ratio(a, b);
        System.Console.WriteLine("sum=" + sum + " ratio=" + ratio);
    }
}
