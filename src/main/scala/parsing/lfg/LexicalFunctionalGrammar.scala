package parsing.lfg

import parsing.AST
import parsing.ContextFreeGrammar

class LexicalFunctionalGrammar[A](
  val productions: Set[LFGProduction[A]],
  val lexicalCategories: Set[LFGLexicalCategory[A]],
  val startSymbol: Option[A] = None) {

  private[this] val cfgProductions = productions.map(_.cfgProduction)
  private[this] val cfgLexicalCategories = lexicalCategories.map(_.cfgLexicalCategory)
  private[this] val cfGrammar =
    new ContextFreeGrammar[A](cfgProductions, cfgLexicalCategories.toList, startSymbol)

  private[this] val productionToSetOfEquations = productions.groupBy(_.cfgProduction).map {
    case (k, lfgProductions) => (k, lfgProductions.map {
      case LFGProduction(_, eqs) => eqs
    })
  }

  def parseTokens(tokens: List[String]): Set[FStructure] = {
    val asts = cfGrammar.parseTokens(tokens)
    /* coming soon
    def annotations(ast: AST[A]): Set[AST[(A, List[Equation[RelativeIdentifier]])]] = ast match {
      case ASTNonterminal(head, children) => {
        val cfgProduction = Production(head, children.map(_.label))
        val equations = productionToSetOfEquations(cfgProduction)
        val childrenAnnotationSets = children.map(annotations _)
        for {
          eq <- equations
          // TODO all combinations of annotations of all children
          childrenAnnotation <- allChildrenAnnotations
        } yield ASTNonterminal((head, eq), )
      }
    }
    */
    ???
  }
}