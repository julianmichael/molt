package parsing

class Parser(
  val tokenizer: Tokenizer,
  val grammar: Grammar) {

  def parsings(s: String): Set[AST] = ???

}