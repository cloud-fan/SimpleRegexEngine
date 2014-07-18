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
  def buildAST(tokens: List[Token]): ASTNode = {
    val operators = new mutable.Stack[Operator]
    val nodes = new mutable.Stack[ASTNode]
    if (tokens.isEmpty) return EmptyNode
    (tokens.head: @unchecked) match {
      case LeftBracketToken => operators.push(LeftBracketOperator)
      case CharToken(v) => nodes.push(ConditionCharNode(v))
      case AnyCharToken => nodes.push(AnyCharNode)
    }

    var previous = tokens.head
    for (token <- tokens.drop(1)) {
      token match {
        case CharToken(v) =>
          mayNeedConcat()
          nodes.push(ConditionCharNode(v))

        case AnyCharToken =>
          mayNeedConcat()
          nodes.push(AnyCharNode)

        case LeftBracketToken =>
          mayNeedConcat()
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
      }

      previous = token
    }

    while (operators.nonEmpty) {
      doOperation(operators.pop())
    }

    def mayNeedConcat() {
      if (previous != LeftBracketToken && previous != OrToken) {
        pushOperator(ConcatOperator)
      }
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
    nodes.head
  }

  def buildNFA(ast: ASTNode) = {

    def buildGroup(node: ASTNode): NFAStatesGroup = {
      node match {
        case n: CharNode => NFAStatesGroup.buildFromCharNode(n)
        case n: ConcatNode => NFAStatesGroup.concat(n.children.map(buildGroup))
        case n: OrNode => NFAStatesGroup.or(n.children.map(buildGroup))
        case n: ZeroOrOneNode => NFAStatesGroup.zeroOrOne(buildGroup(n.child))
        case n: ZeroOrMoreNode => NFAStatesGroup.zeroOrMore(buildGroup((n.child)))
        case n: OneOrMoreNode => NFAStatesGroup.oneOrMore(buildGroup(n.child))
      }
    }

    val group = buildGroup(ast)
    group.end.addTransition(EpsilonTransition(FinalNFAState))
    new NFA(group.start)
  }

}
