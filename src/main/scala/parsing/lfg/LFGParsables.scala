package parsing.lfg
import parsing._
import parsing.cfg._
import parsing.cfg.GenericParsables._
import parsing.cfg.CFGParserHelpers._
import parsing.cfg.CFGParsables._

object LFGParsables {

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
        case FSemanticForm(_, s) => addPrefixedString(prefix, SemanticFormParser.makeString(s))
      }
      (for {
        _ <- prettyForID("", root)
        string <- getString
      } yield string).eval((0, ""))
    }

  }

  implicit object LFGProductionParser extends ComplexCFGParsable[LFGProduction[String]] {

    val ProductionChildParser = new ComplexCFGParsable[(ASTTag[String], Specification)] {
      override val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[(ASTTag[String], Specification)])] = Map(
        List(Alphabetical, Terminal(":"), SpecificationParser) -> (c => for {
          label <- Alphabetical.fromAST(c(0))
          spec <- SpecificationParser.fromAST(c(2))
        } yield (ASTNormalTag(label), spec)),
        List(Terminal("<e>"), Terminal(":"), SpecificationParser) -> (c => for {
          spec <- SpecificationParser.fromAST(c(2))
        } yield (ASTEmptyTag, spec))
      )
    }

    val ProductionChildrenParser = new ListParser(ProductionChildParser)

    override val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[LFGProduction[String]])] = Map(
      List(Alphabetical, Terminal("->"), ProductionChildrenParser) -> (c => for {
        head <- Alphabetical.fromAST(c(0))
        children <- ProductionChildrenParser.fromAST(c(2))
      } yield LFGProduction[String](head, children))
    )
  }

  implicit object LFGLexicalCategoryParser extends ComplexCFGParsable[LFGLexicalCategory[String]] {

    val LexicalEntrySetParser = new SetParser[LexicalEntry](LexicalEntryParser)

    override val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[LFGLexicalCategory[String]])] = Map(
      List(Alphabetical, Terminal(":"), LexicalEntrySetParser) -> (c => for {
        head <- Alphabetical.fromAST(c(0))
        entries <- LexicalEntrySetParser.fromAST(c(2))
      } yield BasicLFGLexicalCategory[String](head, entries))
    )
  }

  implicit object SpecificationParser extends SetParser(EquationParser)

  implicit object LexicalEntryParser extends ComplexCFGParsable[LexicalEntry] {
    override val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[LexicalEntry])] = Map(
      List(Alphabetical) -> (c => for {
        word <- Alphabetical.fromAST(c(0))
      } yield (word, Set[Equation[RelativeIdentifier]]())),
      List(Alphabetical, Terminal(":"), SpecificationParser) -> (c => for {
        word <- Alphabetical.fromAST(c(0))
        eqs <- SpecificationParser.fromAST(c(2))
      } yield (word, eqs.toSet))
    )
  }

  implicit object EquationParser extends ComplexCFGParsable[Equation[RelativeIdentifier]] {
    override val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[Equation[RelativeIdentifier]])] = Map(
      List(Optional(Terminal("(")), Terminal("!"), EquationParser, Optional(Terminal(")"))) -> (c => for {
        eq <- EquationParser.fromAST(c(2))
        leftBrace <- Optional(Terminal("(")).fromAST(c(0))
        rightBrace <- Optional(Terminal(")")).fromAST(c(3))
        if leftBrace.isEmpty == rightBrace.isEmpty
      } yield eq.negation),
      List(Optional(Terminal("(")), EquationParser, Terminal("&"), EquationParser, Optional(Terminal(")"))) -> (c => for {
        left <- EquationParser.fromAST(c(1))
        right <- EquationParser.fromAST(c(3))
        leftBrace <- Optional(Terminal("(")).fromAST(c(0))
        rightBrace <- Optional(Terminal(")")).fromAST(c(4))
        if leftBrace.isEmpty == rightBrace.isEmpty
      } yield Compound(Conjunction(Set(left, right)))),
      List(Optional(Terminal("(")), EquationParser, Terminal("|"), EquationParser, Optional(Terminal(")"))) -> (c => for {
        left <- EquationParser.fromAST(c(1))
        right <- EquationParser.fromAST(c(3))
        leftBrace <- Optional(Terminal("(")).fromAST(c(0))
        rightBrace <- Optional(Terminal(")")).fromAST(c(4))
        if leftBrace.isEmpty == rightBrace.isEmpty
      } yield Compound(Disjunction(Set(left, right)))),
      List(ExpressionParser, Terminal("="), ExpressionParser) -> (c => for {
        left <- ExpressionParser.fromAST(c(0))
        right <- ExpressionParser.fromAST(c(2))
      } yield Defining(Assignment(left, right))),
      List(ExpressionParser, Terminal("<"), ExpressionParser) -> (c => for {
        elem <- ExpressionParser.fromAST(c(0))
        cont <- ExpressionParser.fromAST(c(2))
      } yield Defining(Containment(elem, cont))),
      List(ExpressionParser, Terminal("=c"), ExpressionParser) -> (c => for {
        left <- ExpressionParser.fromAST(c(0))
        right <- ExpressionParser.fromAST(c(2))
      } yield Constraint(Equals(true, left, right))),
      List(ExpressionParser, Terminal("<c"), ExpressionParser) -> (c => for {
        elem <- ExpressionParser.fromAST(c(0))
        cont <- ExpressionParser.fromAST(c(2))
      } yield Constraint(Contains(true, elem, cont))),
      List(ExpressionParser) -> (c => for {
        exp <- ExpressionParser.fromAST(c(0))
      } yield Constraint(Exists(true, exp)))
    )

    def makeString[ID <: Identifier](eq: Equation[ID]): String = eq match {
      case Compound(ceq) => ceq match {
        case Disjunction(eqs) => eqs.map(makeString).mkString(" | ")
        case Conjunction(eqs) => eqs.map(makeString).mkString(" & ")
      }
      case Defining(deq) => deq match {
        case Assignment(lexp, rexp) =>
          s"${ExpressionParser.makeString(lexp)} = ${ExpressionParser.makeString(rexp)}"
        case Containment(lexp, rexp) =>
          s"${ExpressionParser.makeString(lexp)} < ${ExpressionParser.makeString(rexp)}"
      }
      case Constraint(ceq) => ceq match {
        case Equals(pos, lexp, rexp) =>
          s"${if(pos) "" else "!"}${ExpressionParser.makeString(lexp)} =c ${ExpressionParser.makeString(rexp)}"
        case Contains(pos, eexp, cexp) =>
          s"${if(pos) "" else "!"}${ExpressionParser.makeString(eexp)} <c ${ExpressionParser.makeString(cexp)}"
        case Exists(pos, exp) =>
          s"${if(pos) "" else "!"}${ExpressionParser.makeString(exp)}"
      }
    }
  }

  implicit object ExpressionParser extends ComplexCFGParsable[Expression[RelativeIdentifier]] {
    override val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[Expression[RelativeIdentifier]])] = Map(
      List(IdentifyingExpressionParser) -> (c => for {
        exp <- IdentifyingExpressionParser.fromAST(c(0))
      } yield FunctionalExpression(exp)),
      List(ValueCategory) -> (c => for {
        value <- ValueCategory.fromAST(c(0))
      } yield ValueExpression(value)),
      List(
          Terminal("'"), SemanticFormParser, Terminal("'")) -> (c => for {
        sem <- SemanticFormParser.fromAST(c(1))
      } yield SemanticFormExpression(sem))
    )

    def makeString[ID <: Identifier](exp: Expression[ID]): String = exp match {
      case FunctionalExpression(iexp) => IdentifyingExpressionParser.makeString(iexp)
      case ValueExpression(v) => s"$v"
      case SemanticFormExpression(s) => SemanticFormParser.makeString(s)
    }
  }

  implicit object IdentifyingExpressionParser extends ComplexCFGParsable[IdentifyingExpression[RelativeIdentifier]] {
    override val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[IdentifyingExpression[RelativeIdentifier]])] = Map(
      List(RelativeIdentifierParser) -> (c => for {
        id <- RelativeIdentifierParser.fromAST(c(0))
      } yield BareIdentifier(id)),
      List(Optional(Terminal("(")), IdentifyingExpressionParser, Alphabetical, Optional(Terminal(")"))) -> (c => for {
        leftBrace <- Optional(Terminal("(")).fromAST(c(0))
        exp <- IdentifyingExpressionParser.fromAST(c(1))
        feat <- Alphabetical.fromAST(c(2))
        rightBrace <- Optional(Terminal(")")).fromAST(c(3))
        if leftBrace.isEmpty == rightBrace.isEmpty
      } yield Application(exp, feat)),
      List(Optional(Terminal("(")), Alphabetical, IdentifyingExpressionParser, Optional(Terminal(")"))) -> (c => for {
        leftBrace <- Optional(Terminal("(")).fromAST(c(0))
        feat <- Alphabetical.fromAST(c(1))
        exp <- IdentifyingExpressionParser.fromAST(c(2))
        rightBrace <- Optional(Terminal(")")).fromAST(c(3))
        if leftBrace.isEmpty == rightBrace.isEmpty
      } yield InverseApplication(feat, exp))
    )

    def makeString[ID <: Identifier](exp: IdentifyingExpression[ID]): String = exp match {
      case BareIdentifier(x) => s"$x"
      case Application(iexp, feat) => s"(${makeString(iexp)} $feat)"
      case InverseApplication(feat, iexp) => s"($feat ${makeString(iexp)})"
    }
  }

  implicit object SemanticFormParser extends ComplexCFGParsable[SemanticForm] {
    override val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[SemanticForm])] = Map(
      List(Alphabetical) -> (c => for {
        head <- Alphabetical.fromAST(c(0))
      } yield new SemanticForm(head, Nil, Nil)),
      List(Alphabetical, Terminal("<"), FeatureListParser, Terminal(">")) -> (c => for {
        head <- Alphabetical.fromAST(c(0))
        semArgs <- FeatureListParser.fromAST(c(2))
      } yield new SemanticForm(head, semArgs, Nil)),
      List(Alphabetical, Terminal("<"), Optional(FeatureListParser), Terminal(">"), FeatureListParser) -> (c => for {
        head <- Alphabetical.fromAST(c(0))
        semArgsMaybe <- Optional(FeatureListParser).fromAST(c(2))
        semArgs = semArgsMaybe.getOrElse(Nil)
        nonSemArgs <- FeatureListParser.fromAST(c(4))
      } yield new SemanticForm(head, semArgs, nonSemArgs))
    )

    def makeString(sem: SemanticForm): String = sem match { case SemanticForm(head, args, nsArgs) =>
      if(args.isEmpty) s"'$head'"
      else s"'$head<${args.mkString(",")}>${nsArgs.mkString(",")}'"
    }
  }

  implicit object RelativeIdentifierParser extends ComplexCFGParsable[RelativeIdentifier] {
    override val synchronousProductions: Map[List[CFGParsable[_]], (List[AST[CFGParsable[_]]] => Option[RelativeIdentifier])] = Map(
      List(Terminal("up")) -> (c => Some(Up)),
      List(Terminal("down")) -> (c => Some(Down))
    )
  }

  object FeatureListParser extends DelimitedList[Feature](",", Alphabetical)

  object ValueCategory extends CFGParsableLexicalCategory {
    def member(s: String) = s != "up" && s != "down" && s.matches("[A-Za-z0-9+-]+")
  }
}
