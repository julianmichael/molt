package parsing

sealed abstract class AST[A] {
  val production = this match {
    case ASTNonterminal(head, children) => {
      val products = children.map(_.label)
      if(products.isEmpty) None
      else Some(Production(head, products))
    }
    case ASTTerminal(_, _) => None
  }
  val label: A = this match {
    case ASTNonterminal(head, _) => head
    case ASTTerminal(head, _) => head
  }
}
// TODO children should be nonempty list.......maybe? actually maybe not!
// depends on how I want to implement producing the empty string. if at all.
// could either do it with child token "" or no child at all.
case class ASTNonterminal[A](head: A, children: List[AST[A]]) extends AST[A]
case class ASTTerminal[A](head: A, token: String) extends AST[A]

/*
 * AST with only binary and unary productions (encoded in
 * the types) to be used in parsing a CNF* grammar.
 * Distinction is made between productions native to the
 * grammar and nonterminals that were chunked together by the
 * conversion from CFG to CNF grammar. We also have `dechomskify`
 * which converts back to AST.
 */
sealed abstract class CNFAST[A] {
  val flattened: List[CNFAST[A]] = this match {
    case CNFChunkedNonterminal(_, left, right) => left :: right.flattened
    case _                         => this :: Nil
  }
  val dechomskify: Option[AST[A]] = this match {
    case CNFChunkedNonterminal(_, _, _) => None
    case CNFBinaryNonterminal(head, left, right) => {
      val children = (left :: right.flattened).map(_.dechomskify).flatten
      Some(ASTNonterminal[A](head, children))
    }
    case CNFUnaryNonterminal(head, child) => {
      val children = child.flattened.map(_.dechomskify).flatten
      Some(ASTNonterminal[A](head, children))
    }
    case CNFTerminal(head, token) => {
      Some(ASTTerminal[A](head, token))
    }
  }
  val label: CNFTag[A] = this match {
    case CNFChunkedNonterminal(chunk, _, _) => ChunkedTag[A](chunk)
    case CNFBinaryNonterminal(head, _, _) => NormalTag[A](head)
    case CNFUnaryNonterminal(head, _) => NormalTag[A](head)
    case CNFTerminal(head, _) => NormalTag[A](head)
  }
}
case class CNFChunkedNonterminal[A](chunk: List[A], left: CNFAST[A], right: CNFAST[A]) extends CNFAST[A]
case class CNFBinaryNonterminal[A](head: A, left: CNFAST[A], right: CNFAST[A]) extends CNFAST[A]
case class CNFUnaryNonterminal[A](head: A, child: CNFAST[A]) extends CNFAST[A]
case class CNFTerminal[A](head: A, token: String) extends CNFAST[A]