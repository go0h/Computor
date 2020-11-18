package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException

import scala.math.pow

class Operator(left: Expression, op: String, right: Expression) extends Expression {

  override def evaluate: Double = {
    op match {
      case "+" => left.evaluate + right.evaluate
      case "-" => left.evaluate - right.evaluate
      case "*" => left.evaluate * right.evaluate
      case "^" => pow(left.evaluate, right.evaluate)
      case "/" => {
        val r = right.evaluate
        if (r == 0)
          throw new ArithmeticException("Division by zero")
        left.evaluate / right.evaluate
      }
      case "%" => {
        val r = right.evaluate
        if (r == 0)
          throw new ArithmeticException("Modulo by zero")
        left.evaluate % right.evaluate
      }
    }
  }

  override def optimize: Expression = {

    val l = left.optimize
    val r = right.optimize
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
      case "+" => Operator(l, "+", r)
      case "-" => Operator(l, "-", r)
      case _ => throw new EvaluateException(s"Can't optimize expression: $toString")
    }
  }

  override def toString: String = {
    if (left != null && right != null)
     s"($left $op $right)"
    else
      op
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case _: Operator => toString.equals(obj.toString)
//        left.equals(other.left) && op.equals(other.op) && right.equals(other.right)
      case _: Throwable => false
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
