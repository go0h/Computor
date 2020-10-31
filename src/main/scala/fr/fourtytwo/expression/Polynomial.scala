package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException
import fr.fourtytwo.token.{Token, TokenType}

class Polynomial(tokens: Array[Token]) extends Expression {

  if (!tokens.contains(Token("=", TokenType.OPERATION)))
    throw new EvaluateException(s"No equal sign in expression ${tokens.mkString("")}")

  val tokenStrings: Array[String] = tokens.map(x => x.expr)



  val degree = null



  override def evaluate: Double = {
    0.0
  }

}

object Polynomial {
  def apply(tokens: Array[Token]): Polynomial = new Polynomial(tokens)
}