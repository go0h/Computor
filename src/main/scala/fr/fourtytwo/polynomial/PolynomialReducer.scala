package fr.fourtytwo.polynomial

import fr.fourtytwo.RPN
import fr.fourtytwo.exception._
import fr.fourtytwo.expression._
import fr.fourtytwo.token.TokenType.VARIABLE
import fr.fourtytwo.utils.ComputorLogger.LOGGER


object PolynomialReducer {

  def apply(expression: String): Expression = {

    if (expression.count(_ == '=') != 1)
      throw new ParseException(s"No equal sign in expression $expression")

    val tokens = SIMPLE_TOKENIZER.generateTokens(expression).filter(_.tType == VARIABLE)
    val vars = tokens.map(_.expr).distinct

    if (vars.length != 1)
      throw new EvaluateException(s"There is ${vars.length} variables: ${vars.mkString(", ")}. I can't solve")

    LOGGER.fine(s"Original:   $expression")
    simplify(expression)
  }

  def simplify(normExpr: String): Expression = {

    val leftStr = normExpr.substring(0, normExpr.indexOf("="))
    val rightStr = normExpr.substring(normExpr.indexOf("=") + 1)

    val leftExpr = toOptimalExpression(leftStr)
    val rightExpr = toOptimalExpression(rightStr)
    LOGGER.fine(s"Normalized: $leftExpr = $rightExpr")

    val allOnLeft = rightExpr match {
      case r: RealNumber if r.evaluate == 0 => leftExpr
      /* change the sign to all expressions before moving to the left */
      case _ => Operator(leftExpr, "+", rightExpr.changeSign)
    }
    LOGGER.fine(s"Non order:  $allOnLeft = 0.0")

    /* Reorder expression */
    val orderedExpression = exprToArray(allOnLeft)
      .sorted
      .map(_.asInstanceOf[Expression])
      .reduce((x, y) => Operator(x, "+", y))
      .simplify
    LOGGER.fine(s"Ordered:    $orderedExpression = 0.0")

    val fullSimplified = simplifyExpression(orderedExpression)
    LOGGER.info(s"Reduced form: $fullSimplified = 0.0")
    fullSimplified
  }

  /** Convert string expression of polynomial to tree of operations
   *  and simplify it if possible
   */
  def toOptimalExpression(expression: String): Expression = {

    val tokens = SIMPLE_TOKENIZER.generateTokens(expression)

    val beforeOptExpr = RPN(tokens).solve
    simplifyExpression(beforeOptExpr)
  }

  /** Simplification of algebraic expression */
  def simplifyExpression(expr: Expression): Expression = {

    var newExpr: Expression = expr.simplify
    var oldExpr: Expression = RealNumber(0)

    while (!oldExpr.equals(newExpr)) {
      oldExpr = newExpr
      newExpr = newExpr.simplify match {
        case operator: Operator => rotateAndSimplify(operator)
        case operable: Operable => operable
      }
    }
    newExpr
  }

  private def rotateAndSimplify(expr: Operator): Expression = {

    if (expr.contains("*/^%"))
      throw new EvaluateException(s"Can't reduce to normal polynomial form: $expr = 0.0")

    /* Subtraction operators are replaced with addition operators,
     * while changing the sign of the right term. */
    val preSimple: Expression = removeMinus(expr).simplify
    if (preSimple.isInstanceOf[Operable])
      return preSimple

    var curOp = preSimple.asInstanceOf[Operator]

    while (curOp.getLeft.isInstanceOf[Operator] && curOp.getRight.isInstanceOf[Operable]) {

      val left = curOp.getLeft.asInstanceOf[Operator]
      if (left.getRight.isInstanceOf[Operable]) {
        val newOp =
          Operator(left.getLeft,
                   left.getOp,
                   Operator(left.getRight, curOp.getOp, curOp.getRight))

        if (newOp.isInstanceOf[Operable])
          return newOp
        curOp = newOp.asInstanceOf[Operator]
      }
      else
        return curOp.simplify
    }
    curOp.simplify
  }

  def removeMinus(expr: Expression, prevSign: Int = 1): Expression = {
    expr match {
      case vars: Operable => if (prevSign == 1) vars else vars.changeSign
      case op: Operator =>
        val nextSign = if (op.getOp.equals("-")) -1 else 1
        Operator(removeMinus(op.getLeft, prevSign), "+", removeMinus(op.getRight, nextSign))
    }
  }

  def exprToArray(expression: Expression): Array[Operable] = {
    expression match {
      case operable: Operable => Array(operable)
      case op: Operator => exprToArray(op.getLeft) ++ exprToArray(op.getRight)
    }
  }
}
