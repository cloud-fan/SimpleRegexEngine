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

  def concat(groups: List[NFAStatesGroup]) = {
    assert(groups.size > 1)
    var previous = groups.head
    for(g <- groups.drop(1)) {
      previous.end.addTransition(EpsilonTransition(g.start))
      previous = g
    }
    NFAStatesGroup(groups.head.start, groups.last.end)
  }

  def or(groups: List[NFAStatesGroup]) = {
    assert(groups.size > 1)
    for(g <- groups.drop(1)) {
      groups.head.start.addTransition(EpsilonTransition(g.start))
      g.end.addTransition(EpsilonTransition(groups.head.end))
    }
    groups.head
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
