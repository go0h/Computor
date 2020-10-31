package fr.fourtytwo.token

import scala.collection.mutable.{ArrayBuffer, Stack => ScalaStack}
import scala.util.matching.Regex
import fr.fourtytwo.exception.ParseException

class Tokenizer {

  val matchers: Map[TokenType.Value, Regex] = TokenType.getTypesWithRegex

  def generateTokens(expr: String): Array[Token] = {

    var i: Int = 0
    val tokens: ArrayBuffer[Token] = ArrayBuffer[Token]()

    Tokenizer.basicCheckBrackets(expr)

    while (i < expr.length) {

      val token = getToken(expr, i)
      if (token == null)
        throw new ParseException(s"Can't recognize token: $expr", expr.length - i)
      i += token.expr.length
      tokens.append(token)
    }
    tokens.toArray
  }

  private def getToken(expr: String, i: Int): Token = {

    for ((value, matcher) <- matchers) {
      matcher.findPrefixMatchOf(expr.subSequence(i, expr.length)) match {
        case Some(matched) => return Token(expr.substring(i, i + matched.`end`), value)
        case None =>
      }
    }
    null
  }
}

object Tokenizer {

  def apply() = new Tokenizer

  def basicCheckBrackets(expr: String): Unit = {
    val stack = ScalaStack[Char]()

    for (i <- 0 until expr.length) {
      if (expr(i) == '(')
        stack.push(expr(i))
      else if (expr(i) == ')') {
        if (stack.nonEmpty && stack(0) == '(')
          stack.pop()
        else
          throw new ParseException(s"Brackets error: $expr", expr.length - i)
      }
    }
    if (stack.nonEmpty)
      throw new ParseException(s"Brackets error: $expr")
  }
}
