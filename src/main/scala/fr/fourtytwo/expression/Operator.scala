package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException
import fr.fourtytwo.expression.Operator.priority

class Operator(left: Expression, op: String, right: Expression) extends Expression {

  def getOp: String = op
  def getLeft: Expression = left
  def getRight: Expression = right

  def evaluate: Double = OPERATORS(op)(left.evaluate, right.evaluate)

  def simplify: Expression = {

    val l = left.simplify
    val r = right.simplify
    if (l.isInstanceOf[Operator] || r.isInstanceOf[Operator]) {
      val res = if (op.equals("-")) Operator(l, "+", r.changeSign) else Operator(l, op, r)
      return res
    }

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
      case "%" => optL % optR
      case _ => throw new EvaluateException(s"Can't optimize expression: $toString")
    }
  }

  def changeSign: Expression = Operator(left.changeSign, op, right.changeSign)

  override def toString: String = {
    val l = left match {
      case o: Operator => if (priority(o.getOp) < priority(op)) s"($o)" else o.toString
      case o: Operable => o.toString
    }
    val r = right match {
      case o: Operator => if (priority(o.getOp) < priority(op)) s"($o)" else o.toString
      case o: Operable => o.toString
    }
    s"$l $op $r"
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case operator: Operator => toString.equals(operator.toString)
      case _ => false
    }
  }
  def contains(other: String): Boolean = other.contains(op) || left.contains(other) || right.contains(other)
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
    case "--" => 5 //UNARY MINUS
  }

  def leftAssoc(op: String): Boolean = "()-+/*".contains(op)

}
