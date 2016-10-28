package molt.syntax.lfg

import utest._

import molt.syntax._

import molt.syntax.cfg.parsable.ParseCommands._
import molt.syntax.cfg.parsable.CFGParserHelpers._
import molt.syntax.cfg.parsable.CFGParsables._
import molt.syntax.cfg.parsable.GenericParsables._
import molt.syntax.cfg.parsable._

import molt.syntax.lfg.parsable.LFGParsables._

import molt.syntax.cnf._

object LFGTestSuite extends TestSuite {

  def lexCat(symbol: String, lexicalItems: List[String]): LFGLexicalCategory[String] =
    BasicLFGLexicalCategory(symbol, lexicalItems.map(parseForced[LexicalEntry]).toSet)

  // a s{i,a}mple grammar
  val partsOfSpeech = Set(
    lexCat(
      "N", List(
        """man:       up PRED = 'man'""",
        """entity:    up PRED = 'entity'            ,
                      up NUM = SG                   ,
                      up PERS = THD""")),
    lexCat(
      "I", List(
        """did:       up TENSE = PAST               ,
                      up MOOD = DECLARATIVE         ,
                      up VFORM =c BASE              """,
        """to:        ! up TENSE                    """,
        """kissed:    up PRED  = 'kiss<SUBJ,OBJ>'   ,
                      up TENSE = PAST               """
      )),
    lexCat(
      "V", List(
        """seem:      up PRED = 'seem<XCOMP>SUBJ'   ,
                      up SUBJ = up XCOMP SUBJ       ,
                      ((!(up SUBJ NUM = SG &
                          up SUBJ PERS = THD)) &
                          up TENSE = PRESENT) |
                        up VFORM = BASE""",
        """try:       up PRED = 'try<SUBJ,XCOMP>'   ,
                      up SUBJ = up XCOMP SUBJ       ,
                      ((!(up SUBJ NUM = SG &
                          up SUBJ PERS = THD)) &
                          up TENSE = PRESENT) |
                        up VFORM = BASE""",
        """hide:      up PRED = 'hide<SUBJ,OBJ>'    ,
                      ((!(up SUBJ NUM = SG &
                          up SUBJ PERS = THD)) &
                          up TENSE = PRESENT) |
                        up VFORM = BASE""")),
    lexCat(
      "C", List(
        """did:       up TENSE = PAST               ,
                      up MOOD = INTERROGATIVE       ,
                      up VFORM =c BASE""")),
    lexCat(
      "DP", List(
        """I:         up PRED = 'pro'               ,
                      up NUM  = SG                  ,
                      up PERS = FST                 ,
                      up DEF  = yes                 """,
        """John:      up PRED = 'John'              ,
                      up NUM  = SG                  ,
                      up DEF  = yes                 """,
        """Gary:      up PRED = 'Gary'              ,
                      up NUM  = SG                  ,
                      up DEF  = yes                 """)),
    lexCat(
      "A", List(
        """strange:    up PRED = 'strange'          """,
        """green:      up PRED = 'green'            """)),
    lexCat(
      "Adv", List(
        """ quickly:   up PRED = 'quickly'          """)),
    lexCat(
      "D", List(
        """the:       up DEF = yes                 """,
        """a:         up DEF = no                  ,
                      up NUM = SG                  """,
        """what:      up PRED = 'pro'              ,
                      up PRONTYPE = WH             ,
                      (FOC up) MOOD =c INTERROGATIVE""")))
  val productions =
    // Complement
    parseForced[Set[LFGProduction[String]]]("""
      CP ->
        CB:  up = down,

      CP ->
        DP:  up FOC = down,
             up OBJ = down | up XCOMP OBJ = down | up XCOMP XCOMP OBJ = down,
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
             up OBJ = down | up XCOMP OBJ = down,
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
    startSymbols = Set("IP", "CP"),
    wildcards = Map(
      "DF" -> List("TOP", "FOC"),
      "AF" -> List("SUBJ", "OBJ", "OBJR", "OBL", "COMP", "XCOMP")),
    argumentFunctions = Set("SUBJ", "OBJ", "OBJR", "OBL", "COMP", "XCOMP"))

  val parser = new LFGParser(grammar)

  def testSentence(good: Boolean)(implicit testPath: utest.framework.TestPath) = {
    val tokens = testPath.value.last.split(" ").toList
    val fstructs = parser.parseTokens(tokens)
    // println(tokens.mkString(" "))
    // fstructs.foreach(fs => println(FStructureParser.makeString(fs)))
    assert(good != fstructs.isEmpty)
  }

  val tests = this {
    "John kissed Gary" - testSentence(true)
    "a man kissed Gary" - testSentence(true)
    "the man kissed Gary" - testSentence(true)
    "John kissed" - testSentence(false)
    "John kissed Gary a man" - testSentence(false)
    "Gary I kissed" - testSentence(true)
    "what did the entity seem to try to hide" - testSentence(true)
    "did the entity hide Gary" - testSentence(true)
    "what did the strange green entity seem to try to quickly hide" - testSentence(true)
  }
}
