package nfa

import scala.collection.mutable.ListBuffer

/**
 * Created by cloud on 7/17/14.
 */
sealed trait Transition

class EpsilonTransition extends Transition {
  private val _states = new ListBuffer[AbstractNFAState]

  def addState(state: AbstractNFAState) = {
    _states += state
    this
  }

  def addAll(transition: EpsilonTransition) {
    _states ++= transition._states
  }

  def states = _states.toList
}

object EpsilonTransition {
  def apply() = new EpsilonTransition
  def apply(state: AbstractNFAState) = new EpsilonTransition().addState(state)
}

sealed trait PassableTransition extends Transition {
  val target: AbstractNFAState
  def pass(c: Char): Boolean
}

case class ConditionTransition(condition: Char, target: AbstractNFAState) extends PassableTransition {
  def pass(c: Char) = condition == c
}

case class AnyConditionTransition(target: AbstractNFAState) extends PassableTransition {
  def pass(c: Char) = true
}
