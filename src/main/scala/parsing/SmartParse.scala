package parsing

trait SmartParse {
  type SyntaxTree
  type IntermediateRepresentation
  type Score
  val scoreOrdering: Ordering[Score]
  def decorate(tree: SyntaxTree): IntermediateRepresentation
  def score(rep: IntermediateRepresentation): Score
}

object BasicSmartParse {
  type SyntaxTree = cnf.CNFAST
  type IntermediateRepresentation = SyntaxTree
  type Score = Int
  val scoreOrdering = implicitly[Ordering[Int]]
  def decorate(tree: SyntaxTree) = tree
  def score(rep: IntermediateRepresentation): Int = 
}