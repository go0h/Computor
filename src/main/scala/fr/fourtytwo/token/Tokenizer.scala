package fr.fourtytwo.token

import scala.collection.mutable.{ArrayBuffer, Stack => ScalaStack}
import scala.util.matching.Regex
import fr.fourtytwo.exception.ParseException
import fr.fourtytwo.token.TokenType._

class Tokenizer(matchers: Map[TokenType.Value, Regex]) {

  def generateTokens(expr: String): Array[Token] = {

    var i: Int = 0
    val tokens: ArrayBuffer[Token] = ArrayBuffer[Token]()

    if (expr.trim.isEmpty)
      throw new ParseException(s"Empty expression")

    Tokenizer.basicCheckBrackets(expr)

    while (i < expr.length) {

      val token = getToken(expr, i)
      if (token.equals(""))
        throw new ParseException(s"Can't recognize token: $expr", expr.length - i)
      i += token.expr.length
      tokens.append(token)
    }
    tokens.toArray
  }

  private def getToken(expr: String, i: Int): Token = {

    for ((value, matcher) <- matchers) {
      matcher.findPrefixMatchOf(expr.subSequence(i, expr.length)) match {
        case Some(matched) => {
            return Token(expr.substring(i, i + matched.end), value)
        }
        case None =>
      }
    }
    Token("", NONE)
  }
}

object Tokenizer {

  def apply() = new Tokenizer(getMatchers)

  def apply(matchers: Map[TokenType.Value, Regex]) = new Tokenizer(matchers)

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
