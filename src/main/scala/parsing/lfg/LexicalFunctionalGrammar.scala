package parsing.lfg

import parsing.AST
import parsing.ASTNonterminal
import parsing.ASTTerminal
import parsing.Production
import parsing.ContextFreeGrammar

class LexicalFunctionalGrammar[A](
  val productions: Set[LFGProduction[A]],
  val lexicalCategories: Set[LFGLexicalCategory[A]],
  val startSymbol: Option[A] = None) {

  private[this] val cfgProductions = productions.map(_.cfgProduction)
  private[this] val cfgLexicalCategories = lexicalCategories.map(_.cfgLexicalCategory)
  private[this] val cfGrammar =
    new ContextFreeGrammar[A](cfgProductions, cfgLexicalCategories.toList, startSymbol)

  private[this] val productionToSetOfChildSpecifications: Map[Production[A], Set[List[Specification]]] =
    productions.groupBy(_.cfgProduction).map {
      case (k, lfgProductions) => (k, lfgProductions.map {
        case LFGProduction(_, eqs) => eqs.map(_._2)
      })
  }

  private[this] def annotations(ast: AST[A]): Set[AnnotatedAST[A]] = ast match {
    case ASTNonterminal(head, children) => {
      val cfgProduction = Production(head, children.map(_.label))
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
      LFGLexicalCategory(lexicalEntries, symbol) <- lexicalCategories
      if symbol == head
      (word, spec) <- lexicalEntries
      if word == token
    } yield AnnotatedTerminal(head, (token, spec))
  }

  def parseTokens(tokens: List[String]): Set[FStructure] = for {
    ast <- cfGrammar.parseTokens(tokens)
    annotatedAST <- annotations(ast)
    (fdesc, rootID) = annotatedAST.fDescription
    fStruct <- Solution.solve(fdesc, rootID)
  } yield fStruct
}