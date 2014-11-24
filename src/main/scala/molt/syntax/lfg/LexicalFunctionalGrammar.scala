package molt.syntax.lfg

import molt.syntax._
import molt.syntax.cfg._

class LexicalFunctionalGrammar[A](
  val productions: Set[LFGProduction[A]],
  val lexicalCategories: Set[LFGLexicalCategory[A]],
  val startSymbols: Set[A],
  val requireCoherence: Boolean = true,
  val requireCompleteness: Boolean = true,
  val wildcards: Map[Feature, List[Feature]] = Map.empty[Feature, List[Feature]],
  val argumentFunctions: Set[Feature] = Set.empty[Feature]) {

  private[this] val cfgProductions = productions.map(_.cfgProduction)
  private[this] val cfgLexicalCategories: Set[LexicalCategory[A]] = lexicalCategories.collect {
    case (x: LexicalCategory[A]) => x
  }
  val cfGrammar = new ContextFreeGrammar[A](cfgProductions, cfgLexicalCategories, startSymbols)

  private[this] val productionToSetOfChildSpecifications: Map[CFGProduction[A], Set[List[Specification]]] =
    productions.groupBy(_.cfgProduction).map {
      case (k, lfgProductions) => (k, lfgProductions.map {
        case LFGProduction(_, eqs) => eqs.map(_._2)
      })
  }

  def annotations(ast: AST[A]): Set[AnnotatedAST[A]] = ast match {
    case ASTNonterminal(head, children) => {
      val cfgProduction = CFGProduction(head, children.map(_.tag))
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
    case ASTHole(head) => Set(AnnotatedHole(head))
    case ASTEmpty => Set(AnnotatedEmpty)
  }
}