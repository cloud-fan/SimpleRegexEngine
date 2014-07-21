package nfa

import parse.{ConditionCharNode, AnyCharNode, CharNode}

/**
 * Created by cloud on 7/18/14.
 */
case class NFAStatesGroup(val start: NFAState, val end: NFAState)

// detail see: http://www.cnblogs.com/catch/p/3722082.html
object NFAStatesGroup {

  def buildFromCharNode(node: CharNode) = {
    val end = new NFAState
    val start = new NFAState
    val transition = node match {
      case AnyCharNode => new AnyConditionTransition(end)
      case ConditionCharNode(v) => new ConditionTransition(v, end)
    }
    start.addTransition(transition)
    NFAStatesGroup(start, end)
  }

  def concat(left: NFAStatesGroup, right: NFAStatesGroup) = {
    left.end.addTransition(EpsilonTransition(right.start))
    NFAStatesGroup(left.start, right.end)
  }

  def or(left: NFAStatesGroup, right: NFAStatesGroup) = {
    left.start.addTransition(EpsilonTransition(right.start))
    right.end.addTransition(EpsilonTransition(left.end))
    left
  }

  def zeroOrOne(group: NFAStatesGroup) = {
    group.start.addTransition(EpsilonTransition(group.end))
    group
  }

  def zeroOrMore(group: NFAStatesGroup) = {
    group.start.addTransition(EpsilonTransition(group.end))
    group.end.addTransition(EpsilonTransition(group.start))
    group
  }

  def oneOrMore(group: NFAStatesGroup) = {
    group.end.addTransition(EpsilonTransition(group.start))
    group
  }
}
