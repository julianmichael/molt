package parsing

class Parser[A](
  val tokenizer: Tokenizer,
  val grammar: ContextFreeGrammar[A]) {

  def parsings(s: String): Set[AST[A]] = ???

}