package parse

import nfa.{NFA, EpsilonTransition, FinalNFAState, NFAStatesGroup}

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Created by cloud on 7/16/14.
 */
object Parser {

  def compile(regex: String) = {
    buildNFA(buildAST(Token.tokenize(regex)))
  }

  // detail see: http://blog.csdn.net/chinamming/article/details/17175073
  def buildAST(tokens: List[Token]): Option[ASTNode] = {
    val operators = new mutable.Stack[Operator]
    val nodes = new mutable.Stack[ASTNode]
    if (tokens.isEmpty) return None
    (tokens.head: @unchecked) match {
      case LeftBracketToken => operators.push(LeftBracketOperator)
      case CharToken(v) => nodes.push(ConditionCharNode(v))
      case AnyCharToken => nodes.push(AnyCharNode)
    }

    for (token <- tokens.drop(1)) {
      token match {
        case CharToken(v) =>
          nodes.push(ConditionCharNode(v))

        case AnyCharToken =>
          nodes.push(AnyCharNode)

        case LeftBracketToken =>
          operators.push(LeftBracketOperator)

        case RightBracketToken =>
          while (operators.top != LeftBracketOperator) {
            doOperation(operators.pop())
          }
          operators.pop()

        case OneOrMoreToken => pushOperator(OneOrMoreOperator)
        case ZeroOrMoreToken => pushOperator(ZeroOrMoreOperator)
        case ZeroOrOneToken => pushOperator(ZeroOrOneOperator)
        case OrToken => pushOperator(OrOperator)
        case ConcatToken => pushOperator(ConcatOperator)
      }
    }

    while (operators.nonEmpty) {
      doOperation(operators.pop())
    }

    def doOperation(o: Operator) {
      (o: @unchecked) match {
        case ZeroOrOneOperator => nodes.push(ZeroOrOneNode(nodes.pop()))
        case ZeroOrMoreOperator => nodes.push(ZeroOrMoreNode(nodes.pop()))
        case OneOrMoreOperator => nodes.push(OneOrMoreNode(nodes.pop()))
        case OrOperator => nodes.push(new OrNode(nodes.pop(), nodes.pop()))
        case ConcatOperator => nodes.push(new ConcatNode(nodes.pop(), nodes.pop()))
      }
    }

    @tailrec
    def pushOperator(o: Operator) {
      if (operators.isEmpty) {
        operators.push(o)
      } else {
        if (operators.top > o) {
          doOperation(operators.pop())
          pushOperator(o)
        } else {
          operators.push(o)
        }
      }
    }

    assert(nodes.size == 1)
    Some(nodes.head)
  }

  def buildNFA(ast: Option[ASTNode]) = {

    def buildGroup(node: ASTNode): NFAStatesGroup = {
      node match {
        case n: CharNode => NFAStatesGroup.buildFromCharNode(n)
        case n: ConcatNode => NFAStatesGroup.concat(buildGroup(n.left), buildGroup(n.right))
        case n: OrNode => NFAStatesGroup.or(buildGroup(n.left), buildGroup(n.right))
        case n: ZeroOrOneNode => NFAStatesGroup.zeroOrOne(buildGroup(n.child))
        case n: ZeroOrMoreNode => NFAStatesGroup.zeroOrMore(buildGroup((n.child)))
        case n: OneOrMoreNode => NFAStatesGroup.oneOrMore(buildGroup(n.child))
      }
    }

    new NFA(ast.map(n => {
      val group = buildGroup(n)
      group.end.addTransition(EpsilonTransition(FinalNFAState))
      group.start
    }).getOrElse(FinalNFAState))

  }

}
