package fr.fourtytwo

import fr.fourtytwo.expression.Operator.priority

package object expression {

  /** Simplification of algebraic expression */
  def simplifyExpression(expr: Expression): Expression = {

    var oldExpr = expr
    var newExpr = expr.optimize

    while (!oldExpr.equals(newExpr)) {
      oldExpr = newExpr
      newExpr = newExpr.optimize
    }
    newExpr match {
      case operator: Operator => rotateAndSimplify(operator)
      case operable: Operable => operable
    }
  }

  private def rotateAndSimplify(operator: Operator): Expression = {

    var curOp = operator

    while (!curOp.getRight.isInstanceOf[Operator] && curOp.getLeft.isInstanceOf[Operator]) {
      val left = curOp.getLeft.asInstanceOf[Operator]
      if (priority(left.getOp) == priority(curOp.getOp) && left.getRight.isInstanceOf[Operable]) {
        val sign = if (left.getOp.equals("-") && curOp.getOp.equals("+")) "-" else curOp.getOp
        val newOp = Operator(left.getLeft, left.getOp,
                             Operator(left.getRight, sign, curOp.getRight)).optimize
        if (!newOp.isInstanceOf[Operator])
          return newOp
        curOp = newOp.asInstanceOf[Operator]
      }
    }
    curOp
  }



}
