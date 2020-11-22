package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException

import scala.math.pow

class Operator(left: Expression, op: String, right: Expression) extends Expression {

  def getOp: String = op
  def getLeft: Expression = left
  def getRight: Expression = right

  def evaluate: Double = {
    op match {
      case "+" => left.evaluate + right.evaluate
      case "-" => left.evaluate - right.evaluate
      case "*" => left.evaluate * right.evaluate
      case "^" => pow(left.evaluate, right.evaluate)
      case "/" =>
        val r = right.evaluate
        if (r == 0)
          throw new ArithmeticException("Division by zero")
        left.evaluate / r
      case "%" =>
        val r = right.evaluate
        if (r == 0)
          throw new ArithmeticException("Modulo by zero")
        left.evaluate % r
    }
  }

  def simplify: Expression = {

    val l = left.simplify
    val r = right.simplify
    if (l.isInstanceOf[Operator] || r.isInstanceOf[Operator])
      return Operator(l, op, r)

    val optR = r match {
      case r: RealNumber => r
      case v: Variable => v
      case i: Indeterminate => i
    }
    val optL = l match {
        case r: RealNumber => r
        case v: Variable => v
        case i: Indeterminate => i
      }

    op match {
      case "*" => optL * optR
      case "/" => optL / optR
      case "+" => optL + optR
      case "-" => optL - optR
      case "^" => optL ^ optR
      case _ => throw new EvaluateException(s"Can't optimize expression: $toString")
    }
  }

  def changeSign: Expression = Operator(left.changeSign, op, right.changeSign)

  override def toString: String = {
    if (left != null && right != null)
     s"$left $op $right"
    else
      op
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case operator: Operator => toString.equals(operator.toString)
      case _ => false
    }
  }
}

object Operator {

  def apply(left: Expression, op: String, right: Expression): Operator = {
    new Operator(left, op, right)
  }

  def priority: String => Int = {
    case "(" | ")" => 1
    case "-" | "+" => 2
    case "/" | "*" | "%" => 3
    case "^" => 4
    case "--" => 5 // UNARY MINUS
  }
}
