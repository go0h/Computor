package fr.fourtytwo.polynomial

import fr.fourtytwo.RPN
import fr.fourtytwo.exception.{EvaluateException, ParseException}
import fr.fourtytwo.expression.Operator.priority
import fr.fourtytwo.expression.{Expression, Operable, Operator, RealNumber, SIMPLE_TOKENIZER}
import fr.fourtytwo.token.TokenType.VARIABLE

object PolynomialReducer {

  def apply(expression: String): Expression = {
    equalityAndVariableChecks(expression)

    println(s"Original:   $expression")
    simplify(expression)
  }

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
    println(s"Non order:  $allOnLeft = 0.0")

    /* Reorder expression */
    val orderedExpression = exprToArray(allOnLeft)
      .sorted
      .map(_.asInstanceOf[Expression])
      .reduce((x, y) => Operator(x, "+", y))
      .simplify
    println(s"Ordered:    $orderedExpression = 0.0")

    val fullSimplified = simplifyExpression(orderedExpression)
    println(s"Reduced form: $fullSimplified = 0.0")
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

    var oldExpr = expr
    var newExpr = expr.simplify

    while (!oldExpr.equals(newExpr)) {
      oldExpr = newExpr
      newExpr = newExpr.simplify
    }
    newExpr match {
      case operator: Operator => rotateAndSimplify(operator)
      case operable: Operable => operable
    }
  }

  private def rotateAndSimplify(expr: Operator): Expression = {

    if (expr.contains("*") || expr.contains("/") || expr.contains("^"))
      throw new EvaluateException(s"Can't reduce to normal polynomial form: $expr")

    /* Subtraction operators are replaced with addition operators,
 * while changing the sign of the right term. */
    var curOp = removeMinus(expr).asInstanceOf[Operator]

    while (!curOp.getRight.isInstanceOf[Operator] && curOp.getLeft.isInstanceOf[Operator]) {

      val left = curOp.getLeft.asInstanceOf[Operator]

      if (priority(left.getOp) == priority(curOp.getOp) && left.getRight.isInstanceOf[Operable]) {

        val newOp = Operator(left.getLeft,
          left.getOp,
          Operator(left.getRight, curOp.getOp, curOp.getRight))

        if (newOp.isInstanceOf[Operable])
          return newOp
        curOp = newOp.asInstanceOf[Operator]
      }
      else
        return curOp
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
