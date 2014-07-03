package parsing.lfg

sealed abstract class AnnotatedAST[A]
case class AnnotatedNonterminal[A](
  head: A,
  children: List[(AnnotatedAST[A], Specification)])
  extends AnnotatedAST[A]
case class AnnotatedTerminal[A](
  head: A,
  word: LexicalEntry)
  extends AnnotatedAST[A]