package parsing.lfg
import parsing._
import parsing.ParseCommands._

object Parsables {

  object FStructureParser {
    def makeString(fstruct: FStructure): String = fstruct match { case FStructure(map, root) =>
      import scalaz._
      import Scalaz._
      type TabState[A] = State[(Int, String), A]
      def getTabs: TabState[Int] = get[(Int, String)] map { case (int, str) => int }
      def getString: TabState[String] = get[(Int, String)] map { case (int, str) => str }
      def putTabs(tabs: Int): TabState[Unit] =
        get[(Int, String)] flatMap (sta => put(sta.copy(_1 = tabs)))
      def putString(s: String): TabState[Unit] =
        get[(Int, String)] flatMap (sta => put(sta.copy(_2 = s)))
      def indent: TabState[Unit] = for {
        tabs <- getTabs
        _ <- putTabs(tabs + 1)
      } yield ()
      def unindent: TabState[Unit] = for {
        tabs <- getTabs
        _ <- putTabs(tabs - 1)
      } yield ()
      def addPrefixedString(prefix: String, added: String): TabState[Unit] = for {
        tabs <- getTabs
        string <- getString
        indentation = Vector.fill(tabs)("  ").mkString("")
        break = if(string.isEmpty) "" else "\n"
        _ <- putString(s"$string$break$indentation$prefix$added")
      } yield ()
      def addString(added: String): TabState[Unit] = addPrefixedString("", added)
      def prettyForID(prefix: String, id: AbsoluteIdentifier): TabState[Unit] = map(id) match {
        case Empty => addPrefixedString(prefix, "[]")
        case FMapping(m) => for {
          _ <- addPrefixedString(prefix, "[")
          _ <- indent
          _ <- m.foldLeft(get[(Int, String)] map (x => ())){
            case (sta, (feat, subid)) => for {
              _ <- sta
              _ <- prettyForID(s"$feat ==> ", subid)
            } yield ()
          }
          _ <- unindent
          _ <- addString("]")
        } yield ()
        case FSet(s) => for {
          _ <- addPrefixedString(prefix, "{")
          _ <- indent
          _ <- s.foldLeft(get[(Int, String)] map (x => ())) {
            case (sta, subid) => for {
              _ <- sta
              _ <- prettyForID("", subid)
            } yield ()
          }
          _ <- unindent
          _ <- addString("}")
        } yield ()
        case FValue(v) => addPrefixedString(prefix, v)
        case FSemanticForm(s) => addPrefixedString(prefix, SemanticFormParser.makeString(s))
      }
      (for {
        _ <- prettyForID("", root)
        string <- getString
      } yield string).eval((0, ""))
    }

  }
  
  val Specification = DelimitedList(";", EquationParser)
  
  implicit object LexicalEntryParser extends ComplexParsable[LexicalEntry] {
    override val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[LexicalEntry])] = Map(
      List(Alphabetical) -> (c => for {
        word <- Alphabetical.fromAST(c(0))
      } yield (word, Set[Equation[RelativeIdentifier]]())),
      List(Alphabetical, Terminal(":"), Specification) -> (c => for {
        word <- Alphabetical.fromAST(c(0))
        eqs <- Specification.fromAST(c(2))
      } yield (word, eqs.toSet))
    )
  }

  implicit object EquationParser extends ComplexParsable[Equation[RelativeIdentifier]] {
    override val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[Equation[RelativeIdentifier]])] = Map(
      List(Terminal("NOT"), EquationParser) -> (c => for {
        negated <- EquationParser.fromAST(c(1))
      } yield negated.negation),
      List(EquationParser, Terminal("AND"), EquationParser) -> (c => for {
        left <- EquationParser.fromAST(c(0))
        right <- EquationParser.fromAST(c(2))
      } yield Compound(Conjunction(left, right))),
      List(EquationParser, Terminal("OR"), EquationParser) -> (c => for {
        left <- EquationParser.fromAST(c(0))
        right <- EquationParser.fromAST(c(2))
      } yield Compound(Conjunction(left, right))),
      List(Expression, Terminal("="), Expression) -> (c => for {
        left <- Expression.fromAST(c(0))
        right <- Expression.fromAST(c(2))
      } yield Defining(Assignment(left, right))),
      List(Expression, Terminal("IN"), Expression) -> (c => for {
        elem <- Expression.fromAST(c(0))
        cont <- Expression.fromAST(c(2))
      } yield Defining(Containment(elem, cont))),
      List(Expression, Terminal("=c"), Expression) -> (c => for {
        left <- Expression.fromAST(c(0))
        right <- Expression.fromAST(c(2))
      } yield Constraint(Equals(true, left, right))),
      List(Expression, Terminal("INc"), Expression) -> (c => for {
        elem <- Expression.fromAST(c(0))
        cont <- Expression.fromAST(c(2))
      } yield Constraint(Contains(true, elem, cont))),
      List(Expression) -> (c => for {
        exp <- Expression.fromAST(c(0))
      } yield Constraint(Exists(true, exp)))
    )
  }

  object Expression extends ComplexParsable[Expression[RelativeIdentifier]] {
    override val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[Expression[RelativeIdentifier]])] = Map(
      List(IdentifyingExpression) -> (c => for {
        exp <- IdentifyingExpression.fromAST(c(0))
      } yield FunctionalExpression(exp)),
      List(ValueCategory) -> (c => for {
        value <- ValueCategory.fromAST(c(0))
      } yield ValueExpression(value)),
      List(
          Terminal("'"), SemanticFormParser, Terminal("'")) -> (c => for {
        sem <- SemanticFormParser.fromAST(c(1))
      } yield SemanticFormExpression(sem))
    )
  }

  object IdentifyingExpression extends ComplexParsable[IdentifyingExpression[RelativeIdentifier]] {
    override val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[IdentifyingExpression[RelativeIdentifier]])] = Map(
      List(RelativeIdentifier) -> (c => for {
        id <- RelativeIdentifier.fromAST(c(0))
      } yield BareIdentifier(id)),
      List(IdentifyingExpression, Alphabetical) -> (c => for {
        exp <- IdentifyingExpression.fromAST(c(0))
        feat <- Alphabetical.fromAST(c(1))
      } yield Application(exp, feat))
    )
  }

  implicit object SemanticFormParser extends ComplexParsable[SemanticForm] {
    override val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[SemanticForm])] = Map(
      List(Alphabetical) -> (c => for {
        head <- Alphabetical.fromAST(c(0))
      } yield new SemanticForm(head, Nil)),
      List(Alphabetical, Terminal("<"), FeatureList, Terminal(">")) -> (c => for {
        head <- Alphabetical.fromAST(c(0))
        args <- FeatureList.fromAST(c(2))
      } yield new SemanticForm(head, args))
    )

    def makeString(sem: SemanticForm): String = sem match { case SemanticForm(head, args) =>
      if(args.isEmpty) s"'$head'"
      else s"'head<${args.mkString(",")}>'"
    }
  }

  object RelativeIdentifier extends ComplexParsable[RelativeIdentifier] {
    override val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[RelativeIdentifier])] = Map(
      List(Terminal("up")) -> (c => Some(Up)),
      List(Terminal("down")) -> (c => Some(Down))
    )
  }

  val FeatureList = DelimitedList[Feature](",", Alphabetical)

  val ValueCategory =
    ParsableLexicalCategory(s => (s != "up" && s != "down" && s.forall(_.isLetter)))
}
