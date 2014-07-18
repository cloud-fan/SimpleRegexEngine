import parse.{Token, Parser}

/**
 * Created by cloud on 7/18/14.
 */
object FunctionTest extends App {
  val regex = "a(b|c)?a.y*\\.\\??"
  val nfa = Parser.compile(regex)
  println(nfa.matches("abawyy."))
  println(nfa.matches("acat.?"))
  println(nfa.matches("aam."))
  println(nfa.matches("at"))
}
