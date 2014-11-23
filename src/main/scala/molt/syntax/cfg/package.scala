package molt.syntax

package object cfg {
  sealed abstract class CNFConversionTag[A]
  object CNFConversionTag {
    case class Single[A](label: A) extends CNFConversionTag[A]
    case class Chunk[A](label: List[ASTTag[A]]) extends CNFConversionTag[A]

    def convertTag[A](tag: ASTTag[A]): ASTTag[CNFConversionTag[A]] = tag match {
      case ASTNormalTag(x) => ASTNormalTag(Single(x))
      case ASTEmptyTag => ASTEmptyTag
    }
  }
  import CNFConversionTag._
  import cnf._


  private[this] def flattenCNFAST[A](
      cnfAst: CNFAST[CNFConversionTag[A]]): List[CNFAST[CNFConversionTag[A]]] = cnfAst match {
    case CNFBinaryNonterminal(Chunk(_), left, right) => left :: flattenCNFAST(right)
    case CNFHole(Chunk(names))                       => names.map(name => name match {
      case ASTNormalTag(x) => CNFHole(Single(x))
      case ASTEmptyTag => CNFEmpty()
    })
    case _                                           => cnfAst :: Nil
  }

  def convertAST[A](cnfAst: CNFAST[CNFConversionTag[A]]): Option[AST[A]] = cnfAst match {
    case CNFEmpty() => Some(ASTEmpty)
    case CNFHole(Single(head)) => Some(ASTHole(head))
    case CNFBinaryNonterminal(Single(head), left, right) => {
      // TODO change to sequence and return none if there was a failure
      val children = (left :: flattenCNFAST(right)).map(convertAST).flatten
      Some(ASTNonterminal(head, children))
    }
    case CNFUnaryNonterminal(Single(head), child) => {
      // TODO change to sequence and return none if there was a failure
      val children = flattenCNFAST(child).map(convertAST).flatten
      Some(ASTNonterminal[A](head, children))
    }
    case CNFTerminal(Single(head), token) => {
      Some(ASTTerminal[A](head, token))
    }
    case _ => None
  }

  def productionToCNF[A](
      prod: CFGProduction[A]): Set[CNFProduction[CNFConversionTag[A]]] = prod match {
    case CFGProduction(head, children) =>
      collapseProduction(Single(head), children)
  }

  private[this] def collapseProduction[A](
      label: CNFConversionTag[A],
      symbols: List[ASTTag[A]]): Set[CNFProduction[CNFConversionTag[A]]] = symbols match {
    case Nil                    => throw new AssertionError(s"empty production: label $label")
    case child :: Nil           => Set(Unary(label, convertTag(child)))
    case left :: right :: Nil   => Set(Binary(label, convertTag(left), convertTag(right)))
    case left :: remainder      => {
      collapseProduction(Chunk(remainder), remainder) +
          Binary(label, convertTag(left), ASTNormalTag(Chunk(remainder)))
    }
  }

  case class CNFProxyLexicalCategory[A](lexcat: LexicalCategory[A]) extends LexicalCategory[CNFConversionTag[A]] {
    override val symbol = Single(lexcat.symbol)
    override def member(str: String): Boolean = lexcat.member(str)
  }
}