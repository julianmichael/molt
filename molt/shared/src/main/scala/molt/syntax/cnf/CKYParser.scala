package molt.syntax.cnf

import molt.syntax.LexicalCategory
import molt.syntax.cfg.ASTTag
import molt.syntax.cfg.ASTNormalTag
import molt.syntax.cfg.ASTEmptyTag
import molt.util.Memoize

class CKYParser[A](cnfGrammar: CNFGrammar[A]) {

  val productions = cnfGrammar.productions
  val lexicalCategories = cnfGrammar.lexicalCategories
  val startSymbols = cnfGrammar.startSymbols
  
  private[this] type Symbol = ASTTag[A]

  private[this] val allTags =
    productions.flatMap(_.tags) ++ lexicalCategories.map(x => ASTNormalTag(x.symbol))
  private[this] val allLabels =
    productions.flatMap(_.symbols) ++ lexicalCategories.map(_.symbol)

  private[this] lazy val labelToDerivationsWithHole: Map[A, Set[CNFAST[A]]] = {
    def derivationsWithProhibitedRoots(prohib: Set[A], subtree: CNFAST[A]): Set[CNFAST[A]] = {
      val symbol = subtree.tag
      val oneLevelTreesWithLabels = productions.flatMap (prod => prod match {
        case Unary(label, child) if child == symbol && !prohib(label) =>
          Set[(CNFAST[A], A)]((CNFUnaryNonterminal(label, subtree), label))
        case Binary(label, left, right) if !prohib(label) => {
          val lefters = if(left == subtree.tag) { for {
            emptyRightDerivation <- tagsToNullParseTrees(right)
          } yield (CNFBinaryNonterminal(label, subtree, emptyRightDerivation), label) }
          else Set.empty[(CNFAST[A], A)]
          val righters = if(right == subtree.tag) { for {
            emptyLeftDerivation <- tagsToNullParseTrees(left)
          } yield (CNFBinaryNonterminal(label, emptyLeftDerivation, subtree), label) }
          else Set.empty[(CNFAST[A], A)]
          lefters ++ righters
        }
        case _ => Set.empty[(CNFAST[A], A)]
      })
      oneLevelTreesWithLabels.flatMap { case (tree, label) =>
        derivationsWithProhibitedRoots(prohib + label, tree) } + subtree
    }
    allLabels.map(label =>
        (label, derivationsWithProhibitedRoots(Set(label), CNFHole(label)))
    ).toMap
  }

  private[this] def unitParses(subtree: CNFAST[A]): Set[CNFAST[A]] = subtree.tag match {
    case ASTEmptyTag => Set.empty[CNFAST[A]] // don't you dare! they's null parses.
    case ASTNormalTag(x) => {
      val derivations = labelToDerivationsWithHole(x)
      derivations.flatMap(_.attachAtHole(subtree))
    }
  }

  private[this] val tagsToNullParseTrees: Map[Symbol, Set[CNFAST[A]]] = {
    var tagToProducers = allTags.map(tag => (tag, productions.filter {
      case Unary(_, c) if c == tag => true
      case Binary(_, l, r) if l == tag || r == tag => true
      case _ => false
    })).toMap
    var nullable =
      if(allTags(ASTEmptyTag)) Set[(Set[A], CNFAST[A])]((Set.empty[A], CNFEmpty()))
      else Set.empty[(Set[A], CNFAST[A])]
    var todo = nullable
    while(!todo.isEmpty) {
      val b = todo.head
      todo = todo - b
      val (prohib, subtree) = b
      tagToProducers(subtree.tag).foreach {
        case Unary(a, _) if !prohib(a) => {
          val entry = (prohib + a, CNFUnaryNonterminal(a, subtree))
          nullable = nullable + entry
          todo = todo + entry
        }
        case Binary(a, x, y) if !prohib(a) => {
          if(x == subtree.tag) {
            nullable.foreach {
              case (proh, rightTree) if !proh(a) && rightTree.tag == y => {
                val entry = (prohib ++ proh + a, CNFBinaryNonterminal(a, subtree, rightTree))
                nullable = nullable + entry
                todo = todo + entry
              }
              case _ => ()
            }
          }
          if(y == subtree.tag) {
            nullable.foreach {
              case (proh, leftTree) if !proh(a) && leftTree.tag == x => {
                val entry = (prohib ++ proh + a, CNFBinaryNonterminal(a, leftTree, subtree))
                nullable = nullable + entry
                todo = todo + entry
              }
              case _ => ()
            }
          }
        }
        case _ => ()
      }
    }
    val nullParses = nullable.map(_._2)
    nullParses.groupBy(_.tag).withDefaultValue(Set.empty[CNFAST[A]])
  }

  private[this] val unitAncestors: Map[A, Set[Symbol]] =
    labelToDerivationsWithHole.map { case (sym, set) => (sym -> set.map(_.tag)) }

  private[this] def cykTable(tokens: Seq[String]): (((Int, Int)) => Set[Symbol]) = {
    def getEntryGen(recurse: (((Int, Int)) => Set[Symbol]))(indices: (Int, Int)): Set[Symbol] = {
      val (level, offset) = indices
      val lexicalOrBinary: Set[A] = {
        if(level == 0) for { // lexical
          cat <- lexicalCategories
          if cat.member(tokens(offset))
        } yield cat.symbol
        else for { // binary
          (leftIndices, rightIndices) <-
            (1 to level).map(i => ((i - 1, offset), (level - i, offset + i))).toSet
          leftCell = recurse(leftIndices)
          rightCell = recurse(rightIndices)
          Binary(label, leftChild, rightChild) <- productions
          if leftCell(leftChild) && rightCell(rightChild)
        } yield label
      }
      lexicalOrBinary.flatMap(unitAncestors)
    }
    lazy val getEntry: (((Int, Int)) => Set[Symbol]) = Memoize(getEntryGen(getEntry))
    getEntry
  }

  private[this] def makeExtract(
      tokens: Seq[String], table: (((Int, Int)) => Set[Symbol])): (((Symbol, Int, Int)) => Set[CNFAST[A]]) = {
    def extractGen(
        other: (((Symbol, Int, Int)) => Set[CNFAST[A]]))
        (input: (Symbol, Int, Int)): Set[CNFAST[A]] = {
      val (root, level, offset) = input
      other(root, level, offset) ++ (for {
        symb <- table(level, offset)
        tree <- other((symb, level, offset))
        augTree <- unitParses(tree)
        if augTree.tag == root
      } yield augTree)
    }
    def extractAuxGen(
        other: (((Symbol, Int, Int)) => Set[CNFAST[A]]))
        (input: (Symbol, Int, Int)): Set[CNFAST[A]] = {
      val (root, level, offset) = input
      if(table(level, offset)(root)) {
        if (level == 0) { // lexical
          val tok = tokens(offset)
          (for {
            category <- lexicalCategories
            if root == ASTNormalTag(category.symbol)
            if category.member(tok)
          } yield CNFTerminal[A](category.symbol, tok)).toSet
        }
        else for { // binary
          ((leftLevel, leftOffset), (rightLevel, rightOffset)) <-
            (1 to level).map(i => ((i - 1, offset), (level - i, offset + i))).toSet
          leftSymbol <- table((leftLevel, leftOffset))
          rightSymbol <- table((rightLevel, rightOffset))
          rootLabel <- root.toOption.toSet
          if productions(Binary(rootLabel, leftSymbol, rightSymbol))
          leftSubtree <- other((leftSymbol, leftLevel, leftOffset))
          rightSubtree <- other((rightSymbol, rightLevel, rightOffset))
        } yield CNFBinaryNonterminal(rootLabel, leftSubtree, rightSubtree)
      }
      else Set.empty[CNFAST[A]]
    }
    lazy val (extract: (((Symbol, Int, Int)) => Set[CNFAST[A]]),
              extractAux: (((Symbol, Int, Int)) => Set[CNFAST[A]])) =
      (Memoize(extractGen(extractAux)), Memoize(extractAuxGen(extract)))
    extract
  }

  def parseTokens(tokens: Seq[String]): Set[CNFAST[A]] = {
    if(tokens.length == 0) {
      startSymbols.map(ASTNormalTag(_)).flatMap(tagsToNullParseTrees)
    } else {
      val table = cykTable(tokens)
      val extract = makeExtract(tokens, table)
      startSymbols.map(ASTNormalTag(_)).flatMap(sym => extract((sym, tokens.length - 1, 0)))
    }
  }
}
