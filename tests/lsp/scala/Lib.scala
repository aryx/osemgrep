// Helper object for --lsp Scala test.
// Lib.add() returns Int, Lib.ratio() returns Double.
// These types are only known to the LSP server (Metals).

object Lib {
  def add(a: Int, b: Int): Int = a + b
  def ratio(a: Int, b: Int): Double = a.toDouble / b
}
