package molt.syntax.cfg.parsable

import molt.syntax.cfg.AST

/*****
 * Parser Helpers are convenience classes to be used when making parsers. They
 * are meant not to be implicit, as they wouldn't necessarily be the preferred
 * natural way to parse strings into a data type (like List[A] below) and there
 * will be multiple different ways of parsing strings into a data type (i.e.,
 * again, List[A] below). They can implement the functionality of the normal
 * regex kind of stuff you see in typical grammar notation (like the +, *,
 * optionality, etc: TODO get the parser to work with the empty string so * and
 * opt can work) but they are more powerful: in general, they are simply
 * abstractions over parsing structures; i.e., we don't just have a list of A
 * concatenated together (in the case of Plus (+)), but also a DELIMITED list is
 * possible to easily create.
 *
 * The ParserHelpers object keeps these objects out of scope unless you wish to
 * use them.
*****/

object CFGParserHelpers {

  // lexical category consisting only of one string
  case class Terminal(token: String) extends CFGParsableLexicalCategory {
    override def member(s: String) = s == token
    override val tokens = Set(token)
  }

  class RegexLexicalCategory(regex: String) extends CFGParsableLexicalCategory {
    override def member(s: String) = s.matches(regex)
  }
  object RegexLexicalCategory {
    def apply(regex: String): RegexLexicalCategory = new RegexLexicalCategory(regex)
  }

  case object Alphabetical extends RegexLexicalCategory("[a-zA-Z]+")
  case object Alphanumeric extends RegexLexicalCategory("[a-zA-Z0-9]+")
  object Numeric extends RegexLexicalCategory("[0-9]+")

  case class Plus[A](parsable: CFGParsable[A]) extends ComplexCFGParsable[List[A]] {
    final val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[List[A]])] = Map(
      List(parsable, this) -> ( c =>
        for {
          head <- parsable.fromAST(c(0))
          tail <- this.fromAST(c(1))
        } yield head :: tail),
      List(parsable) -> ( c =>
        for {
          end <- parsable.fromAST(c(0))
        } yield List(end)
      ))
  }

  case class DelimitedList[A](delimiter: String, parsable: CFGParsable[A]) extends ComplexCFGParsable[List[A]] {
    final val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[List[A]])] = Map(
      List(parsable, Terminal(delimiter), this) -> ( c =>
        for {
          head <- parsable.fromAST(c(0))
          tail <- this.fromAST(c(2))
        } yield head :: tail),
      List(parsable) -> (c => for {
        end <- parsable.fromAST(c(0))
      } yield List(end))
    )
  }

  case class MultiDelimitedList[A](delimiter: String, parsable: CFGParsable[A]) extends ComplexCFGParsable[List[A]] {
    final val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[List[A]])] = Map(
      List(parsable, Terminal(delimiter), parsable) -> ( c =>
        for {
          fst <- parsable.fromAST(c(0))
          snd <- parsable.fromAST(c(2))
        } yield fst :: snd :: Nil),
      List(parsable, Terminal(delimiter), this) -> ( c =>
        for {
          head <- parsable.fromAST(c(0))
          tail <- this.fromAST(c(2))
        } yield head :: tail)
    )
  }

  case class Optional[A](parsable: CFGParsable[A]) extends ComplexCFGParsable[Option[A]] {
    final val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[Option[A]])] = Map(
      List(parsable) -> (c => for {
        p <- parsable.fromAST(c(0))
      } yield Some(p)),
      List(CFGEmptyCategory) -> (c => Some(None))
    )
  }

  case class Parenthesize[A](inner: CFGParsable[A]) extends ComplexCFGParsable[A] {
    final override val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[A])] = Map(
      List(Terminal("("), inner, Terminal(")")) -> (c => for {
        x <- inner.fromAST(c(1))
      } yield x)
    )
  }

  class OptionallyParenthesize[A](inner: CFGParsable[A]) extends ComplexCFGParsable[A] {
    final override val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[A])] = Map(
      List(Optional(Terminal("(")), inner, Optional(Terminal(")"))) -> (c => for {
        leftBrace <- Optional(Terminal("(")).fromAST(c(0))
        rightBrace <- Optional(Terminal(")")).fromAST(c(2))
        if leftBrace.isEmpty == rightBrace.isEmpty
        x <- inner.fromAST(c(1))
      } yield x)
    )
  }

}
