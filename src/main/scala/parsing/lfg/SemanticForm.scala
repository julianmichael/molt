package parsing.lfg

import parsing._
/*
case class SemanticForm(
  head: String,
  semanticArguments: List[Feature])
object SemanticForm extends ComplexParsable[SemanticForm] {
  val FeatureComma = new ComplexParsable[String] {
    override val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[String])] = Map(
      List(Alphabetical, Terminal(",")) -> (c => for {
        feat <- Alphabetical.fromAST(c(0))
      } yield feat)
    )
  }
  override val synchronousProductions: Map[List[Parsable[_]], (List[AST[Parsable[_]]] => Option[SemanticForm])] = Map(
    List(Alphabetical) -> (c => for {
      head <- Alphabetical.fromAST(c(0))
    } yield SemanticForm(head, Nil, Nil)),
    List(Alphabetical, Terminal("<"), Alphabetical, Terminal(">")) -> (c => for {
      head <- Alphabetical.fromAST(c(0))
      arg <- Alphabetical.fromAST(c(2))
    } yield SemanticForm(head, List(arg))),
    List(Alphabetical, Terminal("<"), Plus(FeatureComma), Alphabetical, Terminal(">")) -> (c => for {
      head <- Alphabetical.fromAST(c(0))
      args <- Plus(FeatureComma).fromAST(c(2))
      arg <- Alphabetical.fromAST(c(3))
    } yield SemanticForm(head, args + arg))
  )
}
*/
