package org.allenai.rulenlg

import simplenlg.features.{Form, Feature}
import simplenlg.framework._
import simplenlg.lexicon._
import simplenlg.phrasespec.SPhraseSpec
import simplenlg.realiser.english._

/**
 * Created by rodneykinney on 8/7/14.
 */
object RuleSentenceGenerator {
  val lexicon = Lexicon.getDefaultLexicon()
  val nlgFactory = new NLGFactory(lexicon)
  val realiser = new Realiser(lexicon)

  def realizeVerbPhrase(phrase: VerbPhrase, useGerund: Boolean = false) = {
    val clause = nlgFactory.createClause
    val subj = phrase.subject map { n => val nc = nlgFactory.createNounPhrase(n); clause.setSubject(nc); nc}
    val verb = phrase.verb map { v => val vc = nlgFactory.createVerbPhrase(v); clause.setVerb(vc); if (useGerund) vc.setFeature(Feature.FORM, Form.GERUND); vc}
    phrase.directObject map clause.setObject
    for ((prep, obj) <- phrase.modifiers) {
      prep match {
        case "arg" => (verb, subj) match {
          case (Some(v), _) => v.addModifier(obj)
          case (_, Some(n)) => n.addModifier(obj)
          case _ => ()
        }
        case _ => clause.addComplement(nlgFactory.createPrepositionPhrase(prep, obj))
      }
    }
    clause
  }

  def asSentence(phrase: VerbPhrase): String = realiser.realiseSentence(realizeVerbPhrase(phrase))

  def asSentence(phrases: Iterable[VerbPhrase]): String = {
    if (phrases.size == 1) {
      asSentence(phrases.head)
    }
    else {
      val clause = nlgFactory.createCoordinatedPhrase
      for (p <- phrases) clause.addCoordinate(realizeVerbPhrase(p))
      realiser.realiseSentence(clause)
    }
  }

  def asFragment(phrase: VerbPhrase, useGerund: Boolean = false) = realiser.realise(realizeVerbPhrase(phrase, useGerund)).toString

  def joinSingle(lhs: VerbPhrase, rhs: VerbPhrase, term: String, useGerundLHS: Boolean = false, useGerundRHS: Boolean = false): String = {
    val lhsClause = realizeVerbPhrase(lhs, useGerundLHS)
    val rhsClause = realizeVerbPhrase(rhs, useGerundRHS)
    rhsClause.setFeature(Feature.COMPLEMENTISER, term)
    lhsClause.addComplement(rhsClause)
    realiser.realiseSentence(lhsClause)
  }

  def join(lhses: Iterable[VerbPhrase], rhs: VerbPhrase, term: String, useGerundLHS: Boolean = false, useGerundRHS: Boolean = false): String = {
    if (lhses.size == 1) {
      joinSingle(lhses.head, rhs, term, useGerundLHS, useGerundRHS)
    }
    else {
      val lhsClause = nlgFactory.createCoordinatedPhrase
      for (lhs <- lhses) lhsClause.addCoordinate(realizeVerbPhrase(lhs, useGerundLHS))

      val rhsClause = realizeVerbPhrase(rhs, useGerundRHS)
      lhsClause.addComplement(s"$term ${realiser.realise(rhsClause)}")
      realiser.realiseSentence(lhsClause)
    }
  }
}
