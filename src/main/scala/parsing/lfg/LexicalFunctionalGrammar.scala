package parsing.lfg

import parsing._
import parsing.cfg._

class LexicalFunctionalGrammar[A](
  val productions: Set[LFGProduction[A]],
  val lexicalCategories: Set[LFGLexicalCategory[A]],
  val startSymbol: Option[A] = None) extends Grammar[FStructure] {

  override def parseTokens(tokens: Seq[String]): Set[FStructure] = for {
    ast <- cfGrammar.parseTokens(tokens)
    annotatedAST <- annotations(ast)
    (fdesc, rootID) = annotatedAST.fDescription
    fStruct <- Solution.solve(fdesc, rootID)
  } yield fStruct

  private[this] val cfgProductions = productions.map(_.cfgProduction)
  val cfGrammar = new ContextFreeGrammar[A](cfgProductions, lexicalCategories.toList, startSymbol)

  private[this] val productionToSetOfChildSpecifications: Map[CFGProduction[A], Set[List[Specification]]] =
    productions.groupBy(_.cfgProduction).map {
      case (k, lfgProductions) => (k, lfgProductions.map {
        case LFGProduction(_, eqs) => eqs.map(_._2)
      })
  }

  private[this] def annotations(ast: AST[A]): Set[AnnotatedAST[A]] = ast match {
    case ASTNonterminal(head, children) => {
      val cfgProduction = CFGProduction(head, children.map(_.label))
      val specifications = productionToSetOfChildSpecifications(cfgProduction)
      val childrenAnnotationSets = children.map(annotations _)
      val annotationChoices = childrenAnnotationSets.foldRight(
        Set[List[AnnotatedAST[A]]](List[AnnotatedAST[A]]())) {
        case (newChoices, setOfLists) => for {
          choice <- newChoices
          list <- setOfLists
        } yield choice :: list
      }
      for {
        spec <- specifications
        childrenAnnotation <- annotationChoices
      } yield AnnotatedNonterminal(head, childrenAnnotation.zip(spec))
    }
    case ASTTerminal(head, token) => for {
      lexcat <- lexicalCategories
      if lexcat.symbol == head
      spec <- lexcat.specifications(token)
    } yield AnnotatedTerminal(head, (token, spec))
  }
}