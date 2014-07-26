package parsing.lfg

import org.scalatest.FunSuite
import parsing._
import parsing.ParseCommands._
import parsing.cfg.GenericParsables._
import parsing.cfg.CFGParserHelpers._
import parsing.lfg.LFGParsables._
import parsing.cnf._

class LFGTestSuite extends FunSuite {

  // a s{i,a}mple grammar
  val partsOfSpeech = parseForced[Set[LFGLexicalCategory[String]]](
    """
        N:      man:        up PRED = 'man'               ,

        I:      kissed:     up PRED  = 'kiss<SUBJ,OBJ>'   ,
                            up TENSE = PAST               ,

        DP:     I:          up PRED = 'pro'               ,
                            up NUM  = SG                  ,
                            up PERS = FST                 ,
                            up DEF  = yes                 ,

                John:       up PRED = 'John'              ,
                            up NUM  = SG                  ,
                            up DEF  = yes                 ,

                Gary:       up PRED = 'Gary'              ,
                            up NUM  = SG                  ,
                            up DEF  = yes                 ,

        D:      the:        up DEF = yes                  ,

                 a:          up DEF = no                  ,
                             up NUM = SG
    """)

  val productions =
    // Inflectional
    parseForced[Set[LFGProduction[String]]]("""
      IP ->
        DP:  up DF = down,
        IP:  up = down,

      IP ->
        DP:  up SUBJ = down,
        IB:  up = down,

      IB ->
        I:   up = down,
        VP:  up = down,

      IB ->
        I:   up = down,

      IB ->
        VP:  up = down
    """) ++
    // Determiner
    //  DP ->
    //    <e>: up DF = down,
    parseForced[Set[LFGProduction[String]]]("""
      DP ->
        DB: up = down,

      DB ->
        D: up = down,

      DB ->
        D:  up = down,
        NP: up = down
    """) ++
    // Noun
    parseForced[Set[LFGProduction[String]]]("""
      NP ->
        NB:  up = down,

      NB ->
        AP: up ADJ = down,
        NB: up = down,

      NB ->
        N:  up = down
    """) ++
    // Adjective
    parseForced[Set[LFGProduction[String]]]("""
      AP ->
        AB: down < up,

      AB ->
        A:  up = down
    """) ++
    // Verb
    parseForced[Set[LFGProduction[String]]]("""
      VP ->
        VB: up = down,

      VB ->
        V:  up = down,
        DP: up OBJ = down,
        DP: up OBJR = down,

      VB ->
        V:  up = down,
        DP: up OBJ = down,

      VB ->
        DP: up OBJ = down,

      VB ->
        DP: up OBJ = down,
        DP: up OBJR = down,

      VB ->
        V:  up = down
    """)

  val governableFunctions = Set("SUBJ", "OBJ", "OBJR", "OBL")

  val grammar = new LexicalFunctionalGrammar[String](
    productions = productions,
    lexicalCategories = partsOfSpeech,
    startSymbol = Some("IP"),
    governableGrammaticalFunctions = governableFunctions)

  def testSentence(tokens: List[String], good: Boolean = true) = {
    println(tokens.mkString(" "))
    val fstructs = grammar.parseTokens(tokens)
    fstructs.map(FStructureParser.makeString).foreach(println)
    println
    val asts = grammar.cfGrammar.parseTokens(tokens)
    asts.foreach(x => println(x.prettyString))
    println
    assert(good != fstructs.isEmpty)
  }

  test(s"John kissed Gary") {
    testSentence(List("John", "kissed", "Gary"))
  }
  test(s"a man kissed Gary") {
    testSentence(List("a", "man", "kissed", "Gary"))
  }
  test(s"the man kissed Gary") {
    testSentence(List("the", "man", "kissed", "Gary"))
  }
  test(s"John kissed") {
    testSentence(List("John", "kissed"), false)
  }
  test(s"John kissed Gary a man") {
    testSentence(List("John", "kissed", "Gary", "a", "man"), false)
  }
  /*
  test(s"Gary I kissed") {
    testSentence(List("Gary", "I", "kissed"))
  }
  */
}