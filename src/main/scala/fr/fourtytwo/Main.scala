package fr.fourtytwo

import fr.fourtytwo.exception._
import fr.fourtytwo.expression._
import fr.fourtytwo.token._

import scala.util.matching.Regex

object Main {

  def optimizeExpression(expr: Expression): Expression = {

    if (expr.isInstanceOf[Operator]) {

    }

    null
  }

  def parseNormalize(expression: String): Unit = {

    val expr = expression.replaceAll("[()\\s]", "")
    val matchers = TokenType.getAllTypesWithRegex
      .filter(x => x._1 != TokenType.VARIABLE
        && x._1 != TokenType.UNARY
        && x._1 != TokenType.NONE)

    val tokenizer = Tokenizer(matchers)
    val tokens = tokenizer.generateTokens(expr).filter(_.tType != TokenType.SPACE)

    val res = RPN(tokens).solve
    optimizeExpression(res)
  }

  def main(args: Array[String]): Unit = {

    val expr = "X^2.0*4-X^1*-4+4=0"
    val expr1 = "4 * X^2 - 3 * X + 5 - X^2 = 3"
    val expr2 = "(4 * y^2) + (4 * y^2) / (-4 * y^2) + (3 * y^3) + 3"

    try {
      val res2 = parseNormalize(expr2)
      println(expr2)
//      println(res2)
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
