// Helper class for --lsp Java test.
// Lib.add() returns int, Lib.ratio() returns double.
// These types are only known to the LSP server (jdtls).

public class Lib {
    public static int add(int a, int b) {
        return a + b;
    }

    public static double ratio(int a, int b) {
        return (double) a / b;
    }
}
