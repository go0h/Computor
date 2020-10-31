package fr.fourtytwo.expression

import scala.math.pow

class Operator(left: Expression, op: String, right: Expression) extends Expression {

  override def evaluate: Double = {
    op match {
      case "+" => left.evaluate + right.evaluate
      case "-" => left.evaluate - right.evaluate
      case "*" => left.evaluate * right.evaluate
      case "/" => left.evaluate / right.evaluate
      case "^" => pow(left.evaluate, right.evaluate)
      case "mod" => left.evaluate % right.evaluate
    }
  }

  override def toString: String = {
    if (left != null && right != null)
     left.toString + op + right.toString
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
    case "/" | "*" | "mod" => 3
    case "^" => 4
  }
}
