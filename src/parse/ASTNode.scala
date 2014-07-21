package parse

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

/**
 * Created by cloud on 7/17/14.
 */
sealed trait ASTNode



sealed trait CharNode extends ASTNode

object AnyCharNode extends CharNode

case class ConditionCharNode(char: Char) extends CharNode



sealed abstract class BinaryNode(val left: ASTNode, val right: ASTNode) extends ASTNode

class ConcatNode(n1: ASTNode, n2: ASTNode) extends BinaryNode(n2, n1)

class OrNode(n1: ASTNode, n2: ASTNode) extends BinaryNode(n2, n1)



sealed trait RepetitionNode extends ASTNode {
  val child: ASTNode
}

case class ZeroOrOneNode(child: ASTNode) extends RepetitionNode

case class ZeroOrMoreNode(child: ASTNode) extends RepetitionNode

case class OneOrMoreNode(child: ASTNode) extends RepetitionNode