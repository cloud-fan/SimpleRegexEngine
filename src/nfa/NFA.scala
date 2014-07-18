package nfa

import scala.collection.mutable

/**
 * Created by cloud on 7/18/14.
 */
class NFA(root: AbstractNFAState) {

  def matches(target: String): Boolean = {
    var currentStates = new mutable.HashSet[AbstractNFAState]
    currentStates += root
    var index = 0
    var matches = false
    while (index < target.size && !matches) {
      val nextStates = new mutable.HashSet[AbstractNFAState]
      for(s <- currentStates) {
        nextStates ++= s.getNextStates(target(index))
      }
      if (nextStates.contains(FinalNFAState)) {
        matches = true
      } else {
        currentStates = nextStates
        index += 1
      }
    }
    matches
  }
}
