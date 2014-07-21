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

  println("------------------------")


  val regex2 = ""
  val nfa2 = Parser.compile(regex2)
  println(nfa2.matches("sad"))
  println(nfa2.matches("eqwe"))
  println(nfa2.matches("sadsda"))
  println(nfa2.matches(""))

}
