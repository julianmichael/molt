package parsing

sealed abstract class AST[+A] {
  val production = this match {
    case ASTParent(head, children) => {
      val products = children collect {
        case ASTParent(label, _) => label
      }
      if(products.isEmpty) None
      else Some(Production(head, products))
    }
    case ASTLeaf(_) => None
  }
  val label: Option[A] = this match {
    case ASTParent(head, _) => Some(head)
    case _ => None
  }
}
// TODO children should be nonempty list.......maybe? actually maybe not!
// depends on how I want to implement producing the empty string. if at all.
// could either do it with child token "" or no child at all.
case class ASTParent[A](head: A, children: List[AST[A]]) extends AST[A]
case class ASTLeaf(token: String) extends AST[Nothing]

/*
 * AST with only binary and unary productions (encoded in
 * the types) to be used in parsing a CNF* grammar.
 * Distinction is made between productions native to the
 * grammar and nonterminals that were chunked together by the
 * conversion from CFG to CNF grammar. We also have `dechomskify`
 * which converts back to AST.
 */
sealed abstract class CNFAST[+A] {
  val flattened: List[CNFAST[A]] = this match {
    case CNFChunkedParent(_, left, right) => left :: right.flattened
    case _                         => this :: Nil
  }
  val dechomskify: Option[AST[A]] = this match {
    case CNFChunkedParent(_, _, _) => None
    case CNFBinaryParent(head, left, right) => {
      val children = (left :: right.flattened).map(_.dechomskify).flatten
      Some(ASTParent[A](head, children))
    }
    case CNFUnaryParent(head, child) => {
      val children = child.flattened.map(_.dechomskify).flatten
      Some(ASTParent[A](head, children))
    }
    case CNFLeaf(token) => {
      Some(ASTLeaf(token))
    }
  }
  val label: Option[CNFTag[A]] = this match {
    case CNFChunkedParent(chunk, _, _) => Some(ChunkedTag[A](chunk))
    case CNFBinaryParent(head, _, _) => Some(NormalTag[A](head))
    case CNFUnaryParent(head, _) => Some(NormalTag[A](head))
    case CNFLeaf(_) => None
  }
}
case class CNFChunkedParent[A](chunk: List[A], left: CNFAST[A], right: CNFAST[A]) extends CNFAST[A]
case class CNFBinaryParent[A](head: A, left: CNFAST[A], right: CNFAST[A]) extends CNFAST[A]
case class CNFUnaryParent[A](head: A, child: CNFAST[A]) extends CNFAST[A]
case class CNFLeaf(token: String) extends CNFAST[Nothing]