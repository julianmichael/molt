package parsing

/*
 * Parsable contains most of the functionality and framework for any object
 * that we want to be able to parse from a string, so I make most of the notes
 * here.
 */
sealed trait Parsable[A] {
  // ----- Must implement -----
  val startSymbol: String

  // This mapping gives us all of the productions for this particular
  // nonterminal, and paired with them are methods to construct one
  // of this nonterminal-associated `A` from its constituent parts.
  val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[A])]

  // ----- May be overridden -----
  // These are the strings that will be regarded as individual words (beyond the
  // normal splitting behavior of the tokenizer). Terminals use these.
  // Non "terminal" lexical categories should not add to this. Tokens cannot be
  // ambiguous; i.e., they cannot overlap with each other so as to lead to
  // multiple possible tokenizations.
  // TODO implement checking for token overlap in tokenizer
  // TODO come up with a better solution than JUST individual token symbols
  val tokens: Set[String] = Set()

  // ----- Cannot be overridden -----

  // List of all of the Parsables that are components of this one (used in productions)
  final lazy val children: Set[Parsable[_]] = {
    val topLayer = synchronousProductions.keySet.flatten - this
    val below = topLayer.flatMap(_.children)
    topLayer ++ below - this
  }

  // all the lexical categories required to parse this Parsable
  final lazy val lexicalCategories: Set[LexicalCategory] = {
    (children + this) collect {
      case c: LexicalCategory => c
    }
  }

  // automatically determine the productions to give the grammar from the
  // synchronous productions of the Parsable and its children
  final lazy val processedSynchronousProductions: Map[Production, (List[AST] => Option[A])] =
    synchronousProductions.map {
      case (k, v) => (RawProduction(startSymbol, k.map(_.startSymbol)), v)
    }

  final lazy val productions: Set[Production] = {
    children.foldRight(processedSynchronousProductions.keySet)(_.productions ++ _)
  }

  final lazy val allTokens: Set[String] =
    children.foldLeft(tokens)(_ ++ _.allTokens)

  // TODO might want something more general than always the basic tokenizer
  final lazy val tokenizer: Tokenizer = new BasicTokenizer(allTokens)

  // the grammar just requires the productions, start symbol, and open symbols
  final lazy val grammar: Grammar = new Grammar(productions, lexicalCategories, Some(startSymbol))

  // automatically get the Parsable from a string; None if it can't be parsed
  final def fromString(s: String) = grammar.parseTokens(tokenizer.tokenize(s)) flatMap fromAST

  final def fromStringUnique(s: String) = {
    val results = fromString(s)
    if (results.size == 1)
      Some(results.head)
    else
      None
  }

  // parse from an abstract syntax tree returned by the parser 
  def fromAST(ast: AST): Option[A]
}

/*
 * ComplexParsable objects rely deterministically on
 * their productions and determine everything from
 * there. The bulk of objects will implement this one.
 */
trait ComplexParsable[A] extends Parsable[A] {
  final def fromAST(ast: AST): Option[A] = {
    for {
      p <- ast.production
      func <- processedSynchronousProductions.get(p)
      item <- func(ast.children)
    } yield item
  }
}

/*
 * SimpleParsable objects are for special cases where
 * we don't want to add any productions to the grammar but we still
 * need to parse stuff from ASTs/strings. This is important for not asploding
 * the grammar size just because we have a large vocabulary.
 */
sealed trait SimpleParsable[A] extends Parsable[A] {
  final val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[A])] = Map()
}

class LexicalCategory(
  override val startSymbol: String,
  val subLexicon: (String => Boolean)) extends SimpleParsable[String] {

  // Lexical categories will always have this structure, looking like POS tags.
  // So even individual terminal symbols will get their own POS-tag type things.
  // I think this makes sense.
  override def fromAST(ast: AST): Option[String] = ast match {
    case BasicASTInternal(`startSymbol`, List(BasicASTLeaf(str))) if subLexicon(str) =>
      Some(str)
    case _ => None
  }
}

// Unary lexical category, consisting only of one string
case class Terminal(override val startSymbol: String)
  extends LexicalCategory(startSymbol, Set(startSymbol)) {
  override val tokens = Set(startSymbol)
}
// Open lexical category, matching any string
// TODO this should be given a more expressive name
case object Word extends LexicalCategory("w", (_ => true))