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

    val res1 = RPN(tokens).solve
    val opt = res1.optimize
    println(res1)
    println(opt)
  }

  def main(args: Array[String]): Unit = {

    val expr = "(8 * y^0.12) * (-4 * y^-0.302) / (2 * y^1) + (-4 * y^3)"

    try {
      parseNormalize(expr)
//      println(res2)
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
