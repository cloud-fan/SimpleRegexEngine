package parse

import scala.collection.mutable

/**
 * Created by cloud on 7/16/14.
 */
sealed trait Token {
  override def toString = this.getClass.getSimpleName
}

case class CharToken(val value: Char) extends Token {
  override def toString = {
    super.toString + ": " + value
  }
}

object AnyCharToken extends Token

object ZeroOrOneToken extends Token

object ZeroOrMoreToken extends Token

object OneOrMoreToken extends Token

object OrToken extends Token

object LeftBracketToken extends Token

object RightBracketToken extends Token

object Token {
  val symbols = Set('(', ')', '.', '?', '*', '+', '|')

  def tokenize(input: String) = {
    val tokens = new mutable.ListBuffer[Token]
    val symmetry = new mutable.Stack[Char]
    var index = 0

    while (index < input.size) {
      val current = input(index)
      current match {
        case '\\' =>
          val next = input(index + 1)
          if (symbols.contains(next)) {
            tokens += CharToken(next)
            index += 1
          } else {
            throw new SyntaxError(s"\\$next is not a valid escape sequence: $index")
          }

        case '(' =>
          symmetry.push(current)
          tokens += LeftBracketToken

        case ')' =>
          if (symmetry.nonEmpty && symmetry.pop == '(') {
            tokens += RightBracketToken
          } else {
            throw new SyntaxError(s"symmetry check failed: $index")
          }

        case '?' => handleRepetitionToken(index, ZeroOrOneToken)

        case '*' => handleRepetitionToken(index, ZeroOrMoreToken)

        case '+' => handleRepetitionToken(index, OneOrMoreToken)

        case '|' => tokens += OrToken

        case '.' => tokens += AnyCharToken

        case _ => tokens += CharToken(current)
      }
      index += 1
    }

    def handleRepetitionToken(index: Int, t: Token) {
      if (tokens.nonEmpty &&
        (tokens.last.isInstanceOf[CharToken] || tokens.last == AnyCharToken || tokens.last == RightBracketToken)) {
        tokens += t
      } else {
        throw new SyntaxError(s"wrong usage of repetition symbol: $index")
      }
    }

    tokens.toList
  }
}
