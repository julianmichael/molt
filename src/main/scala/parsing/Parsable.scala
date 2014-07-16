package parsing

import scala.collection.mutable
import parsing.tokenize._
/*
 * Parsable contains most of the functionality and framework for any object
 * that we want to be able to parse from a string, so I make most of the notes
 * here.
 */
sealed trait Parsable[A] {
  // ----- Must implement -----

  // This mapping gives us all of the productions for this particular
  // nonterminal, and paired with them are methods to construct one
  // of this nonterminal-associated `A` from its constituent parts.
  val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[A])]

  // ----- May be overridden -----

  // These are the strings that will be regarded as individual words (beyond the
  // normal splitting behavior of the tokenizer). Terminals use these.
  // Non "terminal" lexical categories should not add to this. Tokens cannot be
  // ambiguous; i.e., they cannot overlap with each other so as to lead to
  // multiple possible tokenizations.
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
  final lazy val lexicalCategories: List[ParsableLexicalCategory] = {
    ((children + this) collect {
      case (c: ParsableLexicalCategory) => c
    }).toList
  }

  // automatically determine the productions to give the grammar from the
  // synchronous productions of the Parsable and its children
  final lazy val processedSynchronousProductions: Map[Production[Parsable[_]], (List[AST[Parsable[_]]] => Option[A])] =
    synchronousProductions.map {
      case (k, v) => (Production[Parsable[_]](this, k), v)
    }

  final lazy val productions: Set[Production[Parsable[_]]] = {
    children.foldRight(processedSynchronousProductions.keySet)(_.productions ++ _)
  }

  final lazy val allTokens: Set[String] =
    children.foldLeft(tokens)(_ ++ _.allTokens)

  // TODO might want something more general than always the basic tokenizer
  final lazy val tokenizer: Tokenizer = new BasicTokenizer(allTokens)

  // the grammar just requires the productions, lexical categories, and start symbol
  final lazy val grammar: ContextFreeGrammar[Parsable[_]] = new ContextFreeGrammar[Parsable[_]](productions, lexicalCategories, Some(this))

  // automatically get the Parsable from a string; None if it can't be parsed
  final def fromString(s: String) = for {
    tokens <- tokenizer.tokenizations(s)
    ast <- grammar.parseTokens(tokens)
    symbolic <- fromAST(ast)
  } yield symbolic

  final def fromStringUnique(s: String) = {
    val results = fromString(s)
    if (results.size == 1)
      Some(results.head)
    else
      None
  }

  // parse from an abstract syntax tree returned by the parser 
  def fromAST(ast: AST[Parsable[_]]): Option[A]
}

/*
 * ComplexParsable objects rely on
 * their productions and determine everything from
 * there. The bulk of objects will implement this one.
 */
trait ComplexParsable[A] extends Parsable[A] {
  final def fromAST(ast: AST[Parsable[_]]): Option[A] = ast match {
    case ASTNonterminal(head, children) => for {
      p <- Some(Production(head, children.map(_.label)))
      func <- processedSynchronousProductions.get(p)
      item <- func(children)
    } yield item
    case _ => None
  }
}

/*
 * SimpleParsable objects are for special cases where
 * we don't want to add any productions to the grammar but we still
 * need to parse stuff from ASTs/strings. This is important for not asploding
 * the grammar size just because we have a large vocabulary.
 */
sealed trait SimpleParsable[A] extends Parsable[A] {
  final override val synchronousProductions =
    Map.empty[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[A])]
}

class ParsableLexicalCategory(
  val memberFunc: (String => Boolean))
  extends LexicalCategory[Parsable[_]] with SimpleParsable[String] {
  override val symbol = this
  override def member(str: String) = memberFunc(str)
}
object ParsableLexicalCategory {
  def apply(member: (String => Boolean)): ParsableLexicalCategory =
    new ParsableLexicalCategory(member)
}