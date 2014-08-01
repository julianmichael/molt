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
  val partsOfSpeech = Set(
    parseForced[LFGLexicalCategory[String]]("""
        N:      man:        up PRED = 'man'               ,

                entity:     up PRED = 'entity'            ,
                            up NUM = SG                   ,
                            up PERS = THD"""),
    parseForced[LFGLexicalCategory[String]]("""
        I:      did:        up TENSE = PAST               ,
                            up MOOD = DECLARATIVE         ,
                            up VFORM =c BASE              ,

                to:         ! up TENSE                    ,

                kissed:     up PRED  = 'kiss<SUBJ,OBJ>'   ,
                            up TENSE = PAST               """),
    parseForced[LFGLexicalCategory[String]]("""
        V:      seem:       up PRED = 'seem<XCOMP>SUBJ'   ,
                            up SUBJ = up XCOMP SUBJ       ,
      ((up SUBJ NUM = SG & up SUBJ PERS = THD) & up TENSE = PRESENT) | up VFORM = BASE,
                try:        up PRED = 'try<SUBJ,XCOMP>'   ,
                            up SUBJ = up XCOMP SUBJ       ,
      ((up SUBJ NUM = SG & up SUBJ PERS = THD) & up TENSE = PRESENT) | up VFORM = BASE,
                hide:       up PRED = 'hide<SUBJ,OBJ>'    ,
      ((up SUBJ NUM = SG & up SUBJ PERS = THD) & up TENSE = PRESENT) | up VFORM = BASE
                                                          """),
    parseForced[LFGLexicalCategory[String]]("""
        C:      did:        up TENSE = PAST               ,
                            up MOOD = INTERROGATIVE       ,
                            up VFORM =c BASE
                                                          """),
    parseForced[LFGLexicalCategory[String]]("""
        DP:     I:          up PRED = 'pro'               ,
                            up NUM  = SG                  ,
                            up PERS = FST                 ,
                            up DEF  = yes                 ,

                John:       up PRED = 'John'              ,
                            up NUM  = SG                  ,
                            up DEF  = yes                 ,

                Gary:       up PRED = 'Gary'              ,
                            up NUM  = SG                  ,
                            up DEF  = yes                 """),
    parseForced[LFGLexicalCategory[String]]("""
        A:      strange:    up PRED = 'strange'           ,

                green:      up PRED = 'green'
                                                          """),
    parseForced[LFGLexicalCategory[String]]("""
        Adv:    quickly:    up PRED = 'quickly'
                                                         """),
    parseForced[LFGLexicalCategory[String]]("""
        D:      the:        up DEF = yes                  ,

                 a:         up DEF = no                  ,
                            up NUM = SG                  ,

                what:       up PRED = 'pro'              ,
                            up PRONTYPE = WH             ,
                            (FOC up) MOOD = INTERROGATIVE
                """))

  val productions =
    // Complement
    parseForced[Set[LFGProduction[String]]]("""
      CP ->
        DP:  up FOCUS = down,
             up XCOMP XCOMP OBJ = down,
             down PRONTYPE =c WH,
        CB:  up = down,

      CB ->
        C:   up = down,
        IP:  up = down
    """) ++
    // Inflectional
    parseForced[Set[LFGProduction[String]]]("""
      IP ->
        DP:  up TOP = down,
        IP:  up = down,

      IP ->
        DP:  up SUBJ = down,
        IB:  up = down,

      IP ->
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
    parseForced[Set[LFGProduction[String]]]("""
      DP ->
        <e>: (AF up) TOP = up,

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
        AP: down < up ADJ,
        NP:  up = down,

      NP ->
        NB: up = down,

      NB ->
        N:  up = down
    """) ++
    // Adjective
    parseForced[Set[LFGProduction[String]]]("""
      AP ->
        AB: up = down,

      AB ->
        A:  up = down
    """) ++
    // Adverb
    parseForced[Set[LFGProduction[String]]]("""
      AdvP ->
        Adv: up = down
    """) ++
    // Verb
    parseForced[Set[LFGProduction[String]]]("""
      VP ->
        VB: up = down,

      VP ->
        AdvP: down < up ADJ,
        VP:   up = down,

      VB ->
        V:  up = down,

      VB ->
        V:  up = down,
        DP: up OBJ = down,

      VB ->
        DP: up OBJ = down,

      VB ->
        V:  up = down,
        IP: up XCOMP = down
    """)

  val grammar = new LexicalFunctionalGrammar[String](
    productions = productions,
    lexicalCategories = partsOfSpeech,
    startSymbols = Set("IP", "CP"))

  def testSentence(tokens: List[String], good: Boolean = true) = {
    println(tokens.mkString(" "))
    val fstructs = grammar.parseTokens(tokens)
    fstructs.foreach(fs => println(FStructureParser.makeString(fs)))
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
  test(s"Gary I kissed") {
    testSentence(List("Gary", "I", "kissed"))
  }
  /*
  test("what did the strange green entity seem to try to quickly hide") {
    testSentence(List("what","did","the","strange","green","entity","seem","to",
                      "try","to","quickly","hide"))
  }
  */
}