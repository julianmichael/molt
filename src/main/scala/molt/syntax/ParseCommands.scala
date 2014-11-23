package molt.syntax

/* THESE FUNCTIONS ARE THE PREFERRED WAY TO PARSE!
  * The other ways are dispreferred!
  * Kind of, not really. Do your own thing. Whatever.
  * */
object ParseCommands {
  def parse[A](str: String)(implicit parsable: Parsable[A]): Set[A] = for {
    tokens <- parsable.tokenizer.tokenizations(str)
    ast <- parsable.grammar.parseTokens(tokens)
    symbolic <- parsable.fromAST(ast)
  } yield symbolic
  def parseUnique[A](str: String)(implicit parsable: Parsable[A]): Option[A] = {
    val results = parse(str)
    if(results.size == 1) Some(results.head)
    else None
  }
  def parseForced[A](str: String)(implicit parsable: Parsable[A]): A =
    parseUnique(str).get
}
