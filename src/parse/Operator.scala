package parse

/**
 * Created by cloud on 7/17/14.
 */
sealed trait Operator extends Ordered[Operator] {
  val priority: Int

  def compare(o: Operator) = {
    priority - o.priority
  }
}


object OrOperator extends Operator {val priority = 0}

object ConcatOperator extends Operator {val priority = 1}




object ZeroOrOneOperator extends Operator {val priority = 2}

object ZeroOrMoreOperator extends Operator {val priority = 2}

object OneOrMoreOperator extends Operator {val priority = 2}



object LeftBracketOperator extends Operator {val priority = -1}

object RightBracketOperator extends Operator {val priority = -1}

