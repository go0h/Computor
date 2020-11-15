package fr.fourtytwo.expression

import scala.math.pow

class Operator(left: Expression, op: String, right: Expression) extends Expression {

  override def evaluate: Double = {
    op match {
      case "+" => left.evaluate + right.evaluate
      case "-" => left.evaluate - right.evaluate
      case "*" => left.evaluate * right.evaluate
      case "^" => pow(left.evaluate, right.evaluate)
      case "%" => left.evaluate % right.evaluate
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

  override def toString: String = {
    if (left != null && right != null)
     s"($left $op $right)"
    else
      op
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
