package parsing

class Parser[A](
  val tokenizer: Tokenizer,
  val grammar: Grammar[A]) {

  def parsings(s: String): Set[AST[A]] = ???

}