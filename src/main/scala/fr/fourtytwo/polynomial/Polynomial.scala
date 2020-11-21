package fr.fourtytwo.polynomial

import fr.fourtytwo.RPN
import fr.fourtytwo.exception._
import fr.fourtytwo.expression.Operator.priority
import fr.fourtytwo.expression._
import fr.fourtytwo.polynomial.Polynomial._
import fr.fourtytwo.token.TokenType._
import fr.fourtytwo.token._


import scala.collection.mutable.{ArrayBuffer, Stack => ScalaStack}

class Polynomial(expr: String) {

  equalityAndVariableChecks(expr)

  println(s"Original:   $expr")

  val nonOrderPoly: Expression = simplify(expr)

  println(s"Non order:  $nonOrderPoly = 0")

  def simplify(normExpr: String): Expression = {

    val leftStr = normExpr.substring(0, normExpr.indexOf("="))
    val rightStr = normExpr.substring(normExpr.indexOf("=") + 1)

    val leftExpr = toOptimalExpression(leftStr)
    val rightExpr = toOptimalExpression(rightStr)

    println(s"Normalized: $leftExpr = $rightExpr")


    val allOnLeft = rightExpr match {
      case r: RealNumber if r.evaluate == 0 => leftExpr
      /* change the sign to all expressions before moving to the left */
      case _ => Operator(leftExpr, "+", rightExpr.changeSign)
    }
    removeMinus(allOnLeft.optimize)
  }

  val degree = null

  override def toString: String = s"$nonOrderPoly = 0"

}

object Polynomial {

  def apply(expression: String): Polynomial = new Polynomial(expression)

  def toOptimalExpression(expression: String): Expression = {

    val space = Token(" ", TokenType.SPACE)
    val tokens = SIMPLE_TOKENIZER.generateTokens(expression)
    val rpnTokens = RPN.convertToRPN(tokens)

    val normalExpr = normalizeRPNTokens(rpnTokens)
      .replaceAll("[()\\s]", "")

    /* add spaces after each token to recognize unary minus */
    val normalTokens = INDETER_TOKENIZER.generateTokens(normalExpr)
      .flatMap(x => Array(x, space))

    val beforeOptExpr = RPN(normalTokens).solve
    simplifyExpression(beforeOptExpr)
  }

  /** Converts tokens in RPN format to polynomial format expression
   * Example: RPN tokens {{{3 X 2 ^ * => (3.0 * X^2)}}}
   * @return Expression in normalized format
   */
  def normalizeRPNTokens(rpnTokens: Array[Token]): String = {

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
          val degree =
            if (rpnTokens(i + 4).equals("/")) rpnTokens(i+2).expr.toDouble * -1
            else rpnTokens(i+2)
          expressions.append(s"(${rpnTokens(i)} * ${rpnTokens(i+1)}^$degree)")
          i += 5
        }
        // (X 2 ^ 3 *) => (3 * X^2)
        // (X 2 ^ 3 /) => (3 / X^2)
        case Array(V, RN, OP, RN, OP, _*) => {
          if (!rpnTokens(i + 2).equals("^") || (!rpnTokens(i + 4).equals("*") && !rpnTokens(i + 4).equals("/")))
            throw new ParseException(s"Bad indeterminate2 ${rpnTokens.slice(i, i + 5).mkString(" ")}")
          // constant, operator, variable, degree
          val const =
            if (rpnTokens(i + 4).equals("/")) 1.0 / rpnTokens(i+3).expr.toDouble
            else rpnTokens(i+3)
          expressions.append(s"($const * ${rpnTokens(i)}^${rpnTokens(i+1)})")
          i += 5
        }
        // (3 X *) => (3 * X^1)
        // (2 X /) => (0.5 * X^1)
        case Array(RN, V, OP, _*) => {
          if (!rpnTokens(i+2).equals("*") && !rpnTokens(i+2).equals("/"))
            throw new ParseException(s"Bad indeterminate3 ${rpnTokens.slice(i, i + 3).mkString(" ")}")
          // constant, operator, variable, degree
          val const =
            if (rpnTokens(i+2).equals("/")) 1 / rpnTokens(i).expr.toDouble
            else rpnTokens(i)
          expressions.append(s"($const * ${rpnTokens(i+1)}^1)")
          i += 3
        }
        // (X 3 ^) => (1 * X^3)
        // (X 3 *) => (3 * X^1)
        // (X 3 /) => (0.33 * X^1)
        case Array(V, RN, OP, _*) => {
          if (!rpnTokens(i+2).equals("^") && !rpnTokens(i+2).equals("*") && !rpnTokens(i+2).equals("/"))
            throw new ParseException(s"Bad indeterminate4 ${rpnTokens.slice(i, i + 3).mkString(" ")}")
          // constant, operator, variable, degree
          if (rpnTokens(i+2).equals("^"))
            expressions.append(s"(1 * ${rpnTokens(i)}^${rpnTokens(i+1)})")
          else if (rpnTokens(i+2).equals("*"))
            expressions.append(s"(${rpnTokens(i+1)} * ${rpnTokens(i)}^1)")
          else {
            val const = 1 / rpnTokens(i+1).expr.toDouble
            expressions.append(s"($const * ${rpnTokens(i)}^1)")
          }
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

  /** Composite from RPN array expression to normal string */
  def convertToInfix(expressions: Array[String]): String = {

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

  /** Simplification of algebraic expression */
  def simplifyExpression(expr: Expression): Expression = {

    var oldExpr = expr
    var newExpr = expr.optimize

    while (!oldExpr.equals(newExpr)) {
      oldExpr = newExpr
      newExpr = newExpr.optimize
    }
    newExpr match {
      case operator: Operator => rotateAndSimplify(operator).optimize
      case operable: Operable => operable
    }
  }

  private def rotateAndSimplify(operator: Operator): Expression = {

    var curOp = operator

    while (!curOp.getRight.isInstanceOf[Operator] && curOp.getLeft.isInstanceOf[Operator]) {

      val left = curOp.getLeft.asInstanceOf[Operator]

      if (priority(left.getOp) == priority(curOp.getOp) && left.getRight.isInstanceOf[Operable]) {

        val newOp = Operator(left.getLeft,
                             left.getOp,
                             Operator(left.getRight, curOp.getOp, curOp.getRight)).optimize

        if (!newOp.isInstanceOf[Operator])
          return newOp
        curOp = newOp.asInstanceOf[Operator]
      }
    }
    curOp.optimize
  }

  def removeMinus(expr: Expression, prevSign: Int = 1): Expression = {

    if (expr.isInstanceOf[Operable]) {
      return if (prevSign == 1) expr else expr.changeSign
    }

    val op = expr.asInstanceOf[Operator]
    val nextSign = if (op.getOp.equals("-")) -1 else 1

    Operator(removeMinus(op.getLeft, prevSign), "+", removeMinus(op.getRight, nextSign))
  }

  def equalityAndVariableChecks(expression: String): Unit = {

    if (expression.count(_ == '=') != 1)
      throw new ParseException(s"No equal sign in expression $expression")

    val tokens = SIMPLE_TOKENIZER.generateTokens(expression).filter(_.tType == VARIABLE)
    val variables = tokens.map(_.expr).distinct

    if (variables.length != 1)
      throw new EvaluateException(s"There is ${variables.length} " +
        s"variables: ${variables.mkString(", ")}. I can't solve")
  }
}