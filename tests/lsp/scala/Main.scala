// Test file for --lsp with Metals (Scala language server).
//
// The typed metavar pattern ($X: Int) should match expressions
// whose type is 'Int' even when the type can only be determined
// by looking at the definition in another file (Lib.scala).
//
// Without --lsp, semgrep cannot know that Lib.add(a, b) returns Int
// or that Lib.ratio(a, b) returns Double.
// With --lsp, Metals resolves the types across files.

object Main {
  def main(args: Array[String]): Unit = {
    val a: Int = 10
    val b: Int = 20
    // Lib.add is defined in Lib.scala.
    // Only Metals knows Lib.add(a, b) has type Int.
    val sum = Lib.add(a, b)
    // Lib.ratio returns Double, so this should NOT match ($X: Int).
    val ratio = Lib.ratio(a, b)
    println(s"sum=$sum ratio=$ratio")
  }
}
