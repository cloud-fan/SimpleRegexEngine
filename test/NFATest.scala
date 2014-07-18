import parse.{Token, Parser}

/**
 * Created by cloud on 7/18/14.
 */
object NFATest extends App {
  //val regex = "a(b|c)?a.y*\\.\\??"
  val regex = "a(b|c)"
  val nfa = Parser.buildNFA(Parser.buildAST(Token.tokenize(regex)))
  val breakpoint = 1 + 2
}
