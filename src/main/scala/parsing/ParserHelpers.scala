package parsing

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

object ParserHelpers {

  case class Plus[A](parsable: Parsable[A]) extends ComplexParsable[List[A]] {
    final val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[List[A]])] = Map(
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

  case class DelimitedList[A](delimiter: String, parsable: Parsable[A]) extends ComplexParsable[List[A]] {
    final val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[List[A]])] = Map(
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
}
