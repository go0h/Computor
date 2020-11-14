package fr.fourtytwo

import fr.fourtytwo.exception._
import fr.fourtytwo.expression._
import fr.fourtytwo.token._

import scala.util.matching.Regex

object Main {

  def optimizeExpression(expression: Expression): Expression = {
    null
  }

  def parseNormalize(expression: String): Unit = {

    val expr = expression.replaceAll("[()\\s]", "")
    val matchers = TokenType.getAllTypesWithRegex
      .filter(x => x._1 != TokenType.VARIABLE
        && x._1 != TokenType.UNARY
        && x._1 != TokenType.NONE)

    val tokenizer = Tokenizer(matchers)
    val tokens = tokenizer.generateTokens(expr)
      .filter(_.tType != TokenType.SPACE)

    val res = RPN(tokens).solve
    println(s"$res = ${res.evaluate}")
  }

  def main(args: Array[String]): Unit = {

    val tokenizer = Tokenizer()

    val expr = "X^2.0*4-X^1*-4+4=0"
    val expr1 = "4 * X^2 - 3 * X + 5 - X^2 = 3"
    val expr2 = "(4 * y^2) + (4 * y^2) / (-4 * y^2) + (3 * y^3) + 3"
    val expr4 = "4^(-2) / 4" // 4 -2 ^ 3 /
    val expr5 = "4 / 4^(-2)"
    val expr6 = "4 * 4^(2)"
    val expr7 = "4^(-2) / 2"
    val expr8 = "4^(2) / 2"

    try {
      val res5 = RPN(tokenizer.generateTokens(expr5)).solve
      val res6 = RPN(tokenizer.generateTokens(expr6)).solve
      println(s"$res5 = ${res5.evaluate}")
      println(s"$res6 = ${res6.evaluate}")

      val res7 = RPN(tokenizer.generateTokens(expr7)).solve
      val res8 = RPN(tokenizer.generateTokens(expr8)).solve
      println(s"$res7 = ${res7.evaluate}")
      println(s"$res8 = ${res8.evaluate}")
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
