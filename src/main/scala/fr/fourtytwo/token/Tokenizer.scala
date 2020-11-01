package fr.fourtytwo.token

import scala.collection.mutable.{ArrayBuffer, Stack => ScalaStack}
import scala.util.matching.Regex
import fr.fourtytwo.exception.ParseException
import fr.fourtytwo.token.TokenType._

class Tokenizer {

  val matchers: Map[TokenType.Value, Regex] = getTypesWithRegex

  def generateTokens(expr: String): Array[Token] = {

    var i: Int = 0
    val tokens: ArrayBuffer[Token] = ArrayBuffer[Token]()
    val nonSpaceExpr = expr.replaceAll("\\s+", "")

    if (nonSpaceExpr.isEmpty)
      throw new ParseException(s"Empty expression")

    Tokenizer.basicCheckBrackets(nonSpaceExpr)

    while (i < nonSpaceExpr.length) {

      val token = getToken(nonSpaceExpr, i, tokens)
      if (token == null)
        throw new ParseException(s"Can't recognize token: $nonSpaceExpr", nonSpaceExpr.length - i)
      i += token.expr.length
      tokens.append(token)
    }
    tokens.toArray
  }

  private def getToken(expr: String, i: Int, tokens: ArrayBuffer[Token]): Token = {

    for ((value, matcher) <- matchers) {
      matcher.findPrefixMatchOf(expr.subSequence(i, expr.length)) match {
        case Some(matched) => {

          //TODO NEED WORK
          // (-30 - 3)
          if (expr(i) == '-' && value != OPERATION) {
            if (tokens.isEmpty || (tokens.last.tType == OPERATION) && !tokens.last.expr.equals(")")) {
              return Token(expr.substring(i, i + matched.end), value)
            }
          }
          else
            return Token(expr.substring(i, i + matched.end), value)
        }
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
