import parse.{Token, Parser}

/**
 * Created by cloud on 7/18/14.
 */
object ASTTest extends App {
  val regex = "a(b|c)"
  //val regex = "a(b|c)?a.y*\\.\\??"
  val node = Parser.buildAST(Token.tokenize(regex))
  val breakpoint = 2 + 3
}
