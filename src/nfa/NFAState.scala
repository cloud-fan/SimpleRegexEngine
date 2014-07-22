package nfa

import scala.collection.mutable.{ListBuffer, HashSet}

/**
 * Created by cloud on 7/16/14.
 */

sealed trait AbstractNFAState {
  def getNextStates(condition: Char): List[AbstractNFAState]

  def getEpsilonStates: List[AbstractNFAState]
}

object FinalNFAState extends AbstractNFAState {
  def getNextStates(condition: Char) = Nil
  def getEpsilonStates = Nil
}

class NFAState extends AbstractNFAState {

  val passableTransitions = ListBuffer.empty[PassableTransition]
  var epsilonTransition: Option[EpsilonTransition] = None

  def addTransition(transition: Transition) {
    transition match {
      case t: PassableTransition => passableTransitions += t
      case t: EpsilonTransition =>
        if (epsilonTransition == None) {
          epsilonTransition = Some(t)
        } else {
          epsilonTransition.foreach(_.addAll(t))
        }
    }
  }

  def getEpsilonStates = {

    def getAllPaths(state: AbstractNFAState): List[AbstractNFAState] = {
      state match {
        case FinalNFAState => FinalNFAState :: Nil
        case s: NFAState =>
          val result = new HashSet[AbstractNFAState]
          var next = List(state)
          while (next.nonEmpty) {
            result ++= next
            next = (next collect {
              case s: NFAState => s.epsilonTransition.map(_.states.filter(s => !result.contains(s))).getOrElse(Nil)
            }).flatten
          }
          result.toList
      }
    }

    epsilonTransition.map(_.states.flatMap(getAllPaths)).getOrElse(Nil)
  }

  def getNextStates(condition: Char) = {
    val result = new HashSet[AbstractNFAState]
    val next = passableTransitions.filter(_.pass(condition)).map(_.target).toList
    for (s <- next) result ++= s.getEpsilonStates
    result.toList ++ next
  }

}

