import parse.Token

/**
 * Created by cloud on 7/16/14.
 */
object TokenTest extends App {
  val regex = "a(bc)?a.y*\\.\\??"
  Token.tokenize(regex).foreach(println)
}
