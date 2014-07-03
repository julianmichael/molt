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

  private[this] val productionToSetOfChildSpecifications: Map[Production[A], Set[List[Specification]]] = productions.groupBy(_.cfgProduction).map {
    case (k, lfgProductions) => (k, lfgProductions.map {
      case LFGProduction(_, eqs) => eqs.map(_._2)
    })
  }

  def parseTokens(tokens: List[String]): Set[FStructure] = {
    val asts = cfGrammar.parseTokens(tokens)
    def annotations(ast: AST[A]): Set[AnnotatedAST[A]] = ast match {
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
        LexicalEntry(word, spec) <- lexicalEntries
        if word == token
      } yield AnnotatedTerminal(head, LexicalEntry(token, spec))
    }

    val annotatedASTs = asts.flatMap(annotations)

    def fDescription(topAst: AnnotatedAST[A]): FDescription = {
      import scalaz.syntax.state._
      import scalaz.State, State._
      type FDescriptionState[R] = State[Int, R]
      val freshID: FDescriptionState[AbsoluteIdentifier] = for {
        oldName <- get[Int]
        fresh = oldName+1
        _ <- put(fresh)
      } yield AbsoluteIdentifier(s"$fresh")
      def processFDescription(upID: AbsoluteIdentifier, tree: AnnotatedAST[A]): FDescriptionState[FDescription] = tree match {
        case AnnotatedNonterminal(_, children) => {
          val childStates = children.map {
            case (child, spec) => {
              for {
                downID <- freshID
                groundedSpec = spec.map(_.ground(upID, downID))
                childSpec <- processFDescription(downID, child)
              } yield groundedSpec ++ childSpec
            }
          }
          val descListState = childStates.foldRight(
            state[Int, List[FDescription]](List[FDescription]())) {
            case (newDescription, alreadyCalculated) => for {
              prev <- alreadyCalculated
              desc <- newDescription
            } yield desc :: prev
          }
          for {
            descList <- descListState
          } yield descList.reduce(_ ++ _)
        }
        case AnnotatedTerminal(head, lexicalEntry) => lexicalEntry match {
          case LexicalEntry(token, spec) => for {
            downID <- freshID
            groundedSpec = spec.map(_.ground(upID, downID))
          } yield groundedSpec
        }
      }

      val initialID = 0
      val fDescriptionProcessor = for {
        fDesc <- processFDescription(AbsoluteIdentifier(s"$initialID"), topAst)
      } yield fDesc
      fDescriptionProcessor.eval(initialID)
    }

    val fDescriptions = annotatedASTs.map(fDescription _)
    ???
  }
}