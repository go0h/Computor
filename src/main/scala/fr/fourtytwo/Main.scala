package fr.fourtytwo

import fr.fourtytwo.exception._
import fr.fourtytwo.expression._
import fr.fourtytwo.token.{TokenType, _}

import scala.util.matching.Regex

object Main {

//  def parseNormalize(expression: String): Expression = {
//
//    println(s"Base expr:  $expression")
//
//    val tokens = SIMPLE_TOKENIZER.generateTokens(expression)
//    val normalExpr = Polynomial.normalize(RPN.convertToRPN(tokens))
//    println(s"Norm expr1: $normalExpr")
//
//    val normalTokens = INDETER_TOKENIZER
//      .generateTokens(normalExpr.replaceAll("[()\\s]", ""))
//    val beforeOptExpr = RPN(normalTokens).solve
//    val optimize = simplifyExpression(beforeOptExpr)
//    optimize
//  }


  //"8 * y^0.12 * -4 * y^1 + y^1 + -4 * y^3 + 31"
  //((((-32.0 * y^1.12) + (1.0 * y^1.0)) + (-4.0 * y^3.0)) + 31.0)

  def main(args: Array[String]): Unit = {

    val exprL = "8 * y^4 * (4 * y^2) / (2 * y^1) * (3 * y^3)"
    val exprR = "-8 * y^3 + 20 / 4"


    try {
      val left = Polynomial.toOptimalExpression(exprL)
      val right = Polynomial.toOptimalExpression(exprR)
      println(s"$left = $right")
      println(Operator(left, "+", right.changeSign) + " = 0")
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
