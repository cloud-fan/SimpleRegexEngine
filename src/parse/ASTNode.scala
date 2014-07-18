package parse

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

/**
 * Created by cloud on 7/17/14.
 */
sealed trait ASTNode

object EmptyNode extends ASTNode



sealed trait CharNode extends ASTNode

object AnyCharNode extends CharNode

case class ConditionCharNode(char: Char) extends CharNode



sealed abstract class ParentNode extends ASTNode {
  protected val _children = ListBuffer.empty[ASTNode]
  def children = _children.toList
}

class ConcatNode(node2: ASTNode, node1: ASTNode) extends ParentNode {
  node1 match {
    case n1: ConcatNode =>
      _children ++= n1._children
      node2 match {
        case n2: ConcatNode => _children ++= n2._children
        case n2 => _children += n2
      }

    case n1 =>
      _children += n1
      node2 match {
        case n2: ConcatNode => _children ++= n2._children
        case n2 => _children += n2
      }
  }
}

class OrNode(node2: ASTNode, node1: ASTNode) extends ParentNode {
  node1 match {
    case n1: OrNode =>
      _children ++= n1._children
      node2 match {
        case n2: OrNode => _children ++= n2._children
        case n2 => _children += n2
      }

    case n1 =>
      _children += n1
      node2 match {
        case n2: OrNode => _children ++= n2._children
        case n2 => _children += n2
      }
  }
}



sealed trait RepetitionNode extends ASTNode {
  val child: ASTNode
}

case class ZeroOrOneNode(child: ASTNode) extends RepetitionNode

case class ZeroOrMoreNode(child: ASTNode) extends RepetitionNode

case class OneOrMoreNode(child: ASTNode) extends RepetitionNode