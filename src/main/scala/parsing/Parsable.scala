package parsing

/*
 * Parsable contains most of the functionality and framework for any object
 * that we want to be able to parse from a string, so I make most of the notes
 * here. They may seem slightly inaccurate because any object outside this
 * source file will have to mix in ComplexParsable and make use of the Terminal
 * and Word classes for their children to give them the extra functionality that
 * the ComplexParsable trait restricts them from using. However, ComplexParsable
 * makes the process extremely quick and simple.
 */
sealed trait Parsable[A] {
  // ----- Must implement -----
  val startSymbol: String

  // This mapping gives us all of the productions for this particular
  // nonterminal, and paired with them are methods to construct one
  // of this nonterminal-associated `A` from its constituent parts.
  val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[A])]

  // ----- Cannot be overridden -----
  // This may be overridden to specify any terminal symbols of a type that
  // correspond to very "open classes" of words--they may produce any
  // contiguous character sequence that does not consist of the other
  // terminal symbols in the grammar. This is ONLY actually used in Word, below.
  // ASSUMPTION: The open symbols of this Parsable are not (non-open) terminal
  // symbols in the grammar of any of its children.
  // TODO: Come up with a better solution than this!
  // TODO: The better solution... LEXICAL SYMBOLS (aka POS tags. Just list the LexicalCategory things)
  val openSymbols: Set[String] = Set()

  // List of all of the Parsables that are components of this one (used in productions)
  final lazy val children: Set[Parsable[_]] = {
    val topLayer = synchronousProductions.keySet.flatten - this
    val below = topLayer.flatMap(_.children)
    topLayer ++ below - this
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

  private final lazy val allOpenSymbols: Set[String] =
    children.foldLeft(openSymbols)(_ ++ _.allOpenSymbols)

  // the grammar just requires the productions, start symbol, and open symbols
  final lazy val grammar: Grammar = new Grammar(productions, Some(startSymbol), allOpenSymbols)

  // automatically get the Parsable from a string; None if it can't be parsed
  final def fromString(s: String) = grammar.parse(s) flatMap fromAST

  // parse from an abstract syntax tree returned by the parser 
  def fromAST(ast: AST): Option[A]
}

/*
 * ComplexParsable objects rely deterministically on
 * their productions and determine everything from
 * there. The bulk of objects will implement this one.
 */
trait ComplexParsable[A] extends Parsable[A] {
  final override val openSymbols = Set[String]()
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
 * we don't want to add any productions but we still
 * need to parse stuff from ASTs/strings.
 */
sealed trait SimpleParsable[A] extends Parsable[A] {
  final val synchronousProductions: Map[List[Parsable[_]], (List[AST] => Option[A])] = Map()
}

// TODO: Make sure this is actually useful? This is a generalized part of speech, basically
class LexicalCategory(
  override val startSymbol: String,
  val subLexicon: (String => Boolean)) extends SimpleParsable[String] {

  override def fromAST(ast: AST): Option[String] = ast.production match {
    case None if subLexicon(ast.label) => Some(ast.label)
    case _                             => None
  }
}

// Unary lexical category, consisting only of one string
case class Terminal(override val startSymbol: String)
  extends LexicalCategory(startSymbol, Set(startSymbol))
// Open lexical category, matching any string
case object Word extends LexicalCategory("w", (_ => true)) {
  override val openSymbols = Set(startSymbol)
}