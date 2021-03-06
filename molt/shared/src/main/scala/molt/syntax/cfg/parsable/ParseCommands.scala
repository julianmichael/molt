package molt.syntax.cfg.parsable

/* THESE FUNCTIONS ARE THE PREFERRED WAY TO PARSE!
  * The other ways are dispreferred!
  * Kind of, not really. Do your own thing. Whatever.
  * */
object ParseCommands {
  def parse[A](str: String)(implicit parsable: CFGParsable[A]): Stream[A] = for {
    tokens <- parsable.tokenizer.tokenizations(str).toStream
    ast <- parsable.parser.parseTokens(tokens)
    symbolic <- parsable.fromAST(ast)
  } yield symbolic
  def parseFirst[A](str: String)(implicit parsable: CFGParsable[A]): Stream[A] = for {
    tokens <- parsable.tokenizer.tokenizations(str).toStream
    ast <- parsable.parser.parseTokensFirst(tokens)
    symbolic <- parsable.fromAST(ast)
  } yield symbolic
  def parseUnique[A](str: String)(implicit parsable: CFGParsable[A]): Option[A] = {
    val results = parseFirst(str)
    if(results.size == 1) Some(results.head)
    else None
  }
  def parseForced[A](str: String)(implicit parsable: CFGParsable[A]): A =
    parseUnique(str).get
}
