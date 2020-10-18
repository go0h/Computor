package fr.fourtytwo.expression

import scala.math.pow

object Priority extends Enumeration {
  val LOW,
  MEDIUM,
  HIGH = Value(1)
}

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

  override def toString: String = left.toString + op + right.toString
}

object Operator {

  def apply(left: Expression, op: String, right: Expression): Operator = {
    new Operator(left, op, right)
  }

  def priority: String => Priority.Value = {
    case "-" | "+" => Priority.LOW
    case "/" | "*" | "mod" => Priority.MEDIUM
    case "^" => Priority.HIGH
  }
}
