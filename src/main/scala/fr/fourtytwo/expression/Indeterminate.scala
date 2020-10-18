package fr.fourtytwo.expression

import fr.fourtytwo.exception.ParseException

case class Indeterminate(constant: Number,
                         operator: String,
                         variable: Variable,
                         degree: Number) extends Expression {

  def this(c: Double, o: String, v: String, d: Double) {
    this(Number(c), o, Variable(v), Number(d))
  }

  override def evaluate: Double = {
    Operator(constant, operator, Operator(variable, "^", degree)).evaluate
  }

  override def toString: String = {
    constant.toString + operator + variable.toString + "^" + degree.toString
  }
}

object Indeterminate {

  def apply(expression: String): Indeterminate = {

    val prefixPattern = """^([-+]?\d+(?:\.\d+)?)([*\\])([A-Za-z]+)\^(\d+(?:\.\d+)?)$""".r
    val suffixPattern = """(^[A-Za-z]+)\^([0-9]+(?:\.\d+)?)([*\\])(-?[0-9]+(?:\.\d+)?)$""".r

    expression match {
      case prefixPattern(c, op, n, d) => new Indeterminate(c.toDouble, op, n, d.toDouble)
      case suffixPattern(n, d, op, c) => new Indeterminate(c.toDouble, op, n, d.toDouble)
      case _ => throw new ParseException(s"Indeterminate $expression is not well formatted")
    }
  }
}