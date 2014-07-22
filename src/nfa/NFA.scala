package nfa

import scala.collection.mutable

/**
 * Created by cloud on 7/18/14.
 */
class NFA(root: AbstractNFAState) {

  def matches(target: String): Boolean = {

    var currentStates = mutable.HashSet(root)
    currentStates ++= root.getEpsilonStates
    var index = 0
    while (!currentStates.contains(FinalNFAState)) {
      if (index == target.size) return false
      val nextStates = new mutable.HashSet[AbstractNFAState]
      for (s <- currentStates) {
        nextStates ++= s.getNextStates(target(index))
      }
      currentStates = nextStates
      index += 1
    }
    true
  }
}
