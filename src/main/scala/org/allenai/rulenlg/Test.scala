package org.allenai.rulenlg

import spray.json.{DefaultJsonProtocol, JsObject}

/**
 * Created by rodneykinney on 8/7/14.
 */
object Test {

  /*
Prolog: [ "you" "plan" "the foods" ] --EFFECT-> [ "you" "eat" - ]
------------------------------------
Scala:  "you" --HYPONYM-> "healthy"
------------------------------------
Scala:  "it" --HYPONYM-> [ - "healthy" - FOR:"our bodies" ]
------------------------------------
Scala:  [ "we" "exercise" - ARG:"When" ] --CONDITION-> [ "our body" "moving" "the nutrients" ]
Prolog: [ - "get" "rest" ] --PART-> [ - "live" "a healthy life" ]
------------------------------------
Scala:  [ "Our bodies" "need" "time" ] --EFFECT-> [ "Our bodies" "recover" - FROM:"our busy day" ]
Scala:  [ "Our bodies" "need" "time" ] --EFFECT-> [ "Our bodies" "rest" - ]
------------------------------------
Prolog: [ - "give" "the cells" ARG:"time" ] --EFFECT-> "grow"
Prolog: [ - "give" "the cells" ARG:"time" ] --EFFECT-> "repair"
Scala:  [ "our body" "gives" "the cells time" ] --EFFECT-> [ "our body" "grow" - ]
Scala:  [ "our body" "gives" "the cells time" ] --EFFECT-> [ "our body" "repair" - ]
   */
  def main(args: Array[String]) = {
//    val rules = List(
//      HasEffect(VerbPhrase("you", "plan", "the foods"), VerbPhrase("you", "eat")),
//      Causes(VerbPhrase("we", "exercise", "when"), VerbPhrase("our body", "moving", "the nutrients"))
//    )
//    for (r <- rules) println(r.generateSentence)

//
//    for (Rule(id, r, _, _, _) <- allRules) println(r.generateSentence)

//    writeBarronSentences
//    writeQuestionSentences
    writeBarronCandidateSentences
    writeQuestionCandidateSentences
  }

  lazy val allRules = RuleData.read("BarronsRuleDump108Q-1.json")

  lazy val allQuestionRules = RuleData.read("QuestionInterpretationDump108Q-1.json")

  def writeSides(rules: Iterable[Rule], file: String = "rulePhrases.txt") = {
    val w = new java.io.PrintWriter(new java.io.File(file))
    def text(vp: Option[VerbPhrase]): String = vp.map(RuleSentenceGenerator.asFragment(_)).getOrElse("")
    for (Rule(id, r, lhs, rhs, _) <- rules) {
      w.println(s"$id\t${text(lhs)}\t${text(rhs)}")
    }
    w.close
  }

  def writeBarronSentences = writeSentences(allRules, "ruleSentences.txt")
  def writeQuestionSentences = writeSentences(allQuestionRules, "questionRuleSentences.txt")
  def writeBarronCandidateSentences = writeAllCandidateSentences(allRules, "ruleSentenceCandidates.txt")
  def writeQuestionCandidateSentences = writeAllCandidateSentences(allQuestionRules, "questionRuleSentenceCandidates.txt")

  def writeSentences(rules: Iterable[Rule], file: String) = {
    val w = new java.io.PrintWriter(new java.io.File(file))
    w.println(s"id\tgenerated\tsource")
    def text(vp: Option[VerbPhrase]): String = vp.map(RuleSentenceGenerator.asFragment(_)).getOrElse("")
    for (Rule(id, r, lhs, rhs, src) <- rules) {
      w.println(s"$id\t${r.generateSentence}\t$src")
        if (id == "question-rule12572") {
        println(r.generateSentence)
      }
    }
    w.close
  }
  def writeAllCandidateSentences(rules: Iterable[Rule], file: String) = {
    val w = new java.io.PrintWriter(new java.io.File(file))
    w.println(s"id\tgenerated")
    def text(vp: Option[VerbPhrase]): String = vp.map(RuleSentenceGenerator.asFragment(_)).getOrElse("")
    for (Rule(id, r, lhs, rhs, src) <- rules; s <- r.candidateSentences) {
      w.println(s"$id\t$s")

    }
    w.close
  }
}

case class RuleDump(ruleId: String, sourceEnglish: String, prettyString: String, prettyString2: String, ruleString: String, lhs: Option[Side], rhs: Option[Side])

case class Side(subject: Option[String], verb: Option[String], `object`: Option[String], args: Option[List[List[String]]])

object MyProtocol extends DefaultJsonProtocol {
  implicit val sideFormat = jsonFormat4(Side)
  implicit val dumpFormat = jsonFormat7(RuleDump)
}


