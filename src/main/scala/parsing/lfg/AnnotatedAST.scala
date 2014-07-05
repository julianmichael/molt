package parsing.lfg

sealed abstract class AnnotatedAST[A] {
  def fDescription: FDescription = {
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
        case (token, spec) => for {
          downID <- freshID
          groundedSpec = spec.map(_.ground(upID, downID))
        } yield groundedSpec
      }
    }

    val initialID = 0
    val fDescriptionProcessor = for {
      fDesc <- processFDescription(AbsoluteIdentifier(s"$initialID"), this)
    } yield fDesc
    fDescriptionProcessor.eval(initialID)
  }
}

case class AnnotatedNonterminal[A](
  head: A,
  children: List[(AnnotatedAST[A], Specification)])
  extends AnnotatedAST[A]

case class AnnotatedTerminal[A](
  head: A,
  word: LexicalEntry)
  extends AnnotatedAST[A]