package org.allenai.rulenlg

import simplenlg.features.Feature
import spray.json.{JsValue, JsString}

import scala.collection.Iterable
import scala.util.Random
import RuleSentenceGenerator._

/**
 * Created by rodneykinney on 8/7/14.
 */
case class RuleData(id: String, lhs: VerbPhrase, relationship: Relationship, rhs: VerbPhrase)

object RuleData {

  def read(jsonFile: String) = {
    def asLCString(j: JsValue) = j.asInstanceOf[JsString].value.toLowerCase
    def asString(j: JsValue) = j.asInstanceOf[JsString].value
    import spray.json._
    import DefaultJsonProtocol._
    val j = io.Source.fromFile(jsonFile).mkString.parseJson.asInstanceOf[JsArray]
    def extractVerbPhrase(jv: JsValue) = {
      val phraseJson = jv.asJsObject
      val subj: Option[String] = phraseJson.fields.get("subject").map(asLCString)
      val verb: Option[String] = phraseJson.fields.get("verb").map(asLCString)
      val obj: Option[String] = phraseJson.fields.get("object").map(asLCString)
      val args = phraseJson.fields.get("args") map {
        _.asInstanceOf[JsArray].elements.map(_.asInstanceOf[JsArray].elements).map { case List(a, b) => (asLCString(a), asLCString(b))}
      }
      VerbPhrase(subj, verb, obj, args.getOrElse(List()))
    }

    val rules = for {
      ja <- j.elements
      jo = ja.asJsObject
    } yield {
      try {
        val id = asString(jo.fields("ruleId"))
        val src = jo.fields.get("sourceEnglish") map asString
        val lhsOption = jo.fields.get("lhs") map extractVerbPhrase
        val lhs = lhsOption match {
          case Some(l) => List(l)
          case None => {
            jo.fields.get("lhses") match {
              case None => List()
              case Some(j) => j.asInstanceOf[JsArray].elements map extractVerbPhrase
            }
          }
        }
        val rhsOption = jo.fields.get("rhs") map extractVerbPhrase
        val rel: Relationship = rhsOption match {
          case Some(rhs) => {
            val relation = asString(jo.fields("relation"))
            relation match {
              case "EFFECT_OF" => EffectOf(lhs, rhs)
              case "HAS_EFFECT" => HasEffect(lhs, rhs)

              case "ENABLED_BY" => EnabledBy(lhs, rhs)
              case "ENABLES" => Enables(lhs, rhs)

              case "EXAMPLE_OF" => IsExampleOf(lhs, rhs)
              case "HAS_EXAMPLE" => HasExample(lhs, rhs)

              case "HAS_PURPOSE" => HasPurpose(lhs, rhs)
              case "PURPOSE_OF" => PurposeOf(lhs, rhs)

              case "HAS_REQUIREMENT" => HasRequirement(lhs, rhs)
              case "REQUIREMENT_OF" => RequirementOf(lhs, rhs)

              case "CAUSES" => Causes(lhs, rhs)
              case "CAUSED_BY" => CausedBy(lhs, rhs)

              case "THEN" => Then(lhs, rhs)
              case "WHEN" => When(lhs, rhs)

              case "QUERY" => Fact(lhs)

              case _ => Unknown(lhs, rhsOption)
            }
          }
          case None => Fact(lhs)
        }
        Some(Rule(id, rel, lhsOption, rhsOption, src.getOrElse("???")))
      }
      catch {
        case ex: Exception => {
          ex.printStackTrace;
          None
        }
      }
    }
    println(s"Read ${j.elements.size} lines, parsed ${rules.flatten.size} successfully")
    rules.flatten

  }
}

case class Rule(id: String, relationship: Relationship, lhs: Option[VerbPhrase], rhs: Option[VerbPhrase], sourceText: String)

case class VerbPhrase(subject: Option[String], verb: Option[String] = None, directObject: Option[String] = None, modifiers: List[(String, String)] = List())

object VerbPhrase {
  def apply(subject: String, verb: String, directObject: String): VerbPhrase = VerbPhrase(Some(subject), Some(verb), Some(directObject))

  def apply(subject: String, verb: String): VerbPhrase = VerbPhrase(Some(subject), Some(verb))
}

object RNG {
  val rand = new Random
}

sealed trait Relationship {
  def generateSentence: String

  def candidateSentences: Iterable[String]

  def randomElement(elements: IndexedSeq[String]) = {
    elements(RNG.rand.nextInt(elements.size))
  }
}

case class HasEffect(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val (joins, lg, rg) = rhs.verb match {
    case Some(_) => (Array("has the effect that"), true, false)
    case None => (Array("results in"), true, true)
  }

  def generateSentence = join(lhs, rhs, randomElement(joins), lg, rg)

  def candidateSentences: Iterable[String] = joins map (join(lhs, rhs, _, lg, rg))
}

case class EffectOf(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val (joins, lg, rg) = (Array("is an effect of", "results from", "is the result of", "thanks to"), true, true)

  def generateSentence = join(lhs, rhs, randomElement(joins), lg, rg)

  def candidateSentences: Iterable[String] = joins map (join(lhs, rhs, _, lg, rg))
}

case class Enables(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val (joins, lg, rg) = (Array("makes it possible that", "enables", "allows"), true, false)

  def generateSentence = join(lhs, rhs, randomElement(joins), lg, rg)

  def candidateSentences: Iterable[String] = joins map (join(lhs, rhs, _, lg, rg))
}

case class EnabledBy(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val (joins, lg, rg) = (Array("is enabled by"), true, true)

  def generateSentence = join(lhs, rhs, randomElement(joins), lg, rg)

  def candidateSentences: Iterable[String] = joins map (join(lhs, rhs, _, lg, rg))
}

case class Causes(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val (joins, lg, rg) = (Array("causes"), true, true)

  def generateSentence = join(lhs, rhs, randomElement(joins), lg, rg)

  def candidateSentences: Iterable[String] = joins map (join(lhs, rhs, _, lg, rg))
}

case class CausedBy(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val (joins, lg, rg) = (Array("is caused by", "results from", "because of", "thanks to"), true, true)

  def generateSentence = join(lhs, rhs, randomElement(joins), lg, rg)

  def candidateSentences: Iterable[String] = joins map (join(lhs, rhs, _, lg, rg))
}

case class HasRequirement(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val (joins, lg, rg) = rhs.verb match {
    case Some(_) => (Array("requires that", "has the requirement that"), true, false)
    case None => (Array("requires"), true, false)
  }

  def generateSentence = join(lhs, rhs, randomElement(joins), lg, rg)

  def candidateSentences: Iterable[String] = joins map (join(lhs, rhs, _, lg, rg))
}

case class RequirementOf(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val (joins, lg, rg) = (Array("is a requirement for"), true, true)

  def generateSentence = join(lhs, rhs, randomElement(joins), true, true)

  def candidateSentences: Iterable[String] = joins map (join(lhs, rhs, _, lg, rg))
}

case class IsExampleOf(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  def generateSentence = join(lhs, rhs, "is an example of", true, true)

  def candidateSentences: Iterable[String] = List(generateSentence)
}

case class HasExample(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val (joins, lg, rg) = rhs.verb match {
    case Some(v) => (Array("has as an example that"), true, false)
    case None => (Array("has as an example"), true, true)
  }

  def generateSentence = join(lhs, rhs, randomElement(joins), lg, rg)

  def candidateSentences: Iterable[String] = joins map (join(lhs, rhs, _, lg, rg))
}

case class HasPurpose(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val (joins, lg, rg) = rhs.verb match {
    case Some(_) => (Array("has the purpose that"), true, false)
    case None => (Array("has the purpose"), false, false)
  }

  def generateSentence = join(lhs, rhs, randomElement(joins), lg, rg)
  def candidateSentences: Iterable[String] = joins map (join(lhs, rhs, _, lg, rg))
}

case class PurposeOf(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val joins = Array("is the purpose of")

  def generateSentence = join(lhs, rhs, randomElement(joins), true, true)
  def candidateSentences: Iterable[String] = List(generateSentence)
}

case class Then(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val joins = Array("if-then")

  def generateSentence = join(lhs, rhs, randomElement(joins))
  def candidateSentences: Iterable[String] = List(generateSentence)
}

case class When(lhs: Iterable[VerbPhrase], rhs: VerbPhrase) extends Relationship {
  val joins = Array("when")

  def generateSentence = join(lhs, rhs, randomElement(joins))
  def candidateSentences: Iterable[String] = List(generateSentence)
}

case class Fact(phrase: Iterable[VerbPhrase]) extends Relationship {
  def generateSentence = asSentence(phrase)
  def candidateSentences: Iterable[String] = List(generateSentence)
}

case class Unknown(lhs: Iterable[VerbPhrase], rhs: Option[VerbPhrase]) extends Relationship {
  def generateSentence = s"LHS=$lhs, RHS=$rhs"
  def candidateSentences: Iterable[String] = List(generateSentence)
}

/*
Relation:

"EFFECT_OF"
"HAS_EFFECT"

"ENABLED_BY"
"ENABLES"

"EXAMPLE_OF"
"HAS_EXAMPLE"

"HAS_PURPOSE"
"PURPOSE_OF"

"HAS_REQUIREMENT"
"REQUIREMENT_OF"

"CAUSED_BY"
"CAUSES"

"THEN"
"WHEN"

 */

