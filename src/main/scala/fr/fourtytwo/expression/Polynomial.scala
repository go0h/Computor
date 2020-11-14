package fr.fourtytwo.expression

import fr.fourtytwo.RPN
import fr.fourtytwo.token._
import fr.fourtytwo.exception._
import fr.fourtytwo.expression.Polynomial.{createNormalizePolynomial, normalize}
import fr.fourtytwo.token.TokenType._

import scala.collection.mutable.{ArrayBuffer, Stack => ScalaStack}

class Polynomial(expr: String) extends Expression {

  if (expr.count(_ == '=') != 1)
    throw new ParseException(s"No equal sign in expression $expr")

  val normExp: String = createNormalizePolynomial(expr)

  val degree = null

  override def evaluate: Double = {
    0.0
  }

  override def toString: String = expr

}

object Polynomial {

  def apply(expression: String): Polynomial = new Polynomial(expression)

  def createNormalizePolynomial(expression: String): String = {

    val tokenizer: Tokenizer = Tokenizer(getSimpleTypesWithRegex)
    val tokens: Array[Token] = tokenizer.generateTokens(expression)

    if (varNums(tokens) != 1)
      throw new EvaluateException(s"There is ${varNums(tokens)} " +
        s"variables: ${getVars(tokens)}. I can't solve")

    val leftRPN: Array[Token] =
      RPN.convertToRPN(tokens.slice(0, tokens.indexOf(Token("=", OPERATION))))
    val rightRPN: Array[Token] =
      RPN.convertToRPN(tokens.slice(tokens.indexOf(Token("=", OPERATION)) + 1, tokens.length))

    println(s"""Expression: ${tokens.mkString("")}
               |Left RPN  : ${leftRPN.mkString("")}
               |Right RPN : ${rightRPN.mkString("")}""".stripMargin)
    val leftExpr: String = normalize(leftRPN)
    val rightExpr: String = normalize(rightRPN)

    s"$leftExpr = $rightExpr"
  }

  private def convertToInfix(expressions: Array[String]): String = {

    val stack = ScalaStack[String]()

    for (expr <- expressions) {
      expr match {
        case "+" | "*" | "/" | "-" | "^" => {
          if (stack.length < 2)
            throw new EvaluateException(s"Wrong1 ${expr.mkString(" ")}")
          val first = stack.pop()
          val second = stack.pop()
          stack.push(s"$second $expr $first")
        }
        case _ => stack.push(expr)
      }
    }
    if (stack.length != 1)
      throw new EvaluateException(s"Wrong2 ${expressions.mkString(" ")}")
    stack.pop()
  }

  def normalize(rpnTokens: Array[Token]): String = {

    val RN = REALNUMBER
    val V = VARIABLE
    val OP = OPERATION

    var i = 0
    val allTypes = rpnTokens.map(_.tType)
    var tempTypes = allTypes
    val expressions: ArrayBuffer[String] = new ArrayBuffer[String]()

    while (tempTypes.nonEmpty) {
      tempTypes match {
        // (3 X 2 ^ *) => (3 * X^2)
        // (3 X 2 ^ /) => (X^2 / 3)
        case Array(RN, V, RN, OP, OP, _*) => {
          if (!rpnTokens(i + 3).equals("^") || (!rpnTokens(i + 4).equals("*") && !rpnTokens(i + 4).equals("/")))
            throw new ParseException(s"Bad indeterminate1 ${rpnTokens.slice(i, i + 5).mkString(" ")}")

          // constant, operator, variable, degree
          val degree = if (rpnTokens(i + 4).equals("/")) rpnTokens(i+2).expr.toDouble * -1 else rpnTokens(i+2)
          expressions.append(s"(${rpnTokens(i)} * ${rpnTokens(i+1)}^$degree)")
          i += 5
        }
        // (X 2 ^ 3 *) => (3 * X^2)
        // (X 2 ^ 3 /) => (3 / X^2)
        case Array(V, RN, OP, RN, OP, _*) => {
          if (!rpnTokens(i + 2).equals("^") || (!rpnTokens(i + 4).equals("*") && !rpnTokens(i + 4).equals("/")))
            throw new ParseException(s"Bad indeterminate2 ${rpnTokens.slice(i, i + 5).mkString(" ")}")
          // constant, operator, variable, degree

          val const = if (rpnTokens(i + 4).equals("/")) 1.0 / rpnTokens(i+3).expr.toDouble else rpnTokens(i+3)
          expressions.append(s"($const * ${rpnTokens(i)}^${rpnTokens(i+1)})")
          i += 5
        }
        // (3 X *) => (3 * X^1)
        case Array(RN, V, OP, _*) => {
          if (!rpnTokens(i+2).equals("*") && !rpnTokens(i+2).equals("/"))
            throw new ParseException(s"Bad indeterminate3 ${rpnTokens.slice(i, i + 3).mkString(" ")}")
          // constant, operator, variable, degree
          expressions.append(s"(${rpnTokens(i)} ${rpnTokens(i+2)} ${rpnTokens(i+1)}^1)")
          i += 3
        }
        // (X 3 ^) => (1 * X^3)
        case Array(V, RN, OP, _*) => {
          if (!rpnTokens(i+2).equals("^"))
            throw new ParseException(s"Bad indeterminate4 ${rpnTokens.slice(i, i + 3).mkString(" ")}")
          // constant, operator, variable, degree
          expressions.append(s"(1 * ${rpnTokens(i)}^${rpnTokens(i+1)})")
          i += 3
        }
        // (X) => (1 * X^1)
        case Array(V, _*) => {
          expressions.append(s"(1 * ${rpnTokens(i)}^1)")
          i += 1
        }
        case Array(OP, _*) | Array(RN, _*) => {
          expressions.append(rpnTokens(i).expr)
          i += 1
        }
        case _ => throw new EvaluateException(s"Can't solve token ${rpnTokens(i)}")
      }
      tempTypes = allTypes.slice(i, allTypes.length)
    }
    convertToInfix(expressions.toArray)
  }

  private def varNums(infixTokens: Array[Token]): Int = {
    infixTokens
      .filter(_.tType == VARIABLE)
      .map(_.expr)
      .distinct.length
  }

  private def getVars(infixTokens: Array[Token]): String = {
    infixTokens
      .filter(_.tType == VARIABLE)
      .map(x => s"'${x.expr}'")
      .mkString(", ")
  }
}