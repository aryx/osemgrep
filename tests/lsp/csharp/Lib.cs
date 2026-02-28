// Helper class for --lsp C# test.
// Lib.Add() returns int, Lib.Ratio() returns double.
// These types are only known to the LSP server (OmniSharp).

public static class Lib {
    public static int Add(int a, int b) {
        return a + b;
    }

    public static double Ratio(int a, int b) {
        return (double) a / b;
    }
}
