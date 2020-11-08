package fr.fourtytwo.expression

import fr.fourtytwo.exception.ParseException

case class Indeterminate(constant: Expression,
                         operator: String,
                         variable: Variable,
                         degree: Expression) extends Expression {

  def this(c: Double, o: String, v: String, d: Double) {
    this(RealNumber(c), o, Variable(v), RealNumber(d))
  }

  override def evaluate: Double = {
    Operator(constant, operator, Operator(variable, "^", degree)).evaluate
  }

  override def toString: String = {
    constant.toString + operator + variable.toString + "^" + degree.toString
  }

  override def equals(other: Any): Boolean = {

    other match {
      case temp: Number => {
        if (this.degree.evaluate == 0 && temp.evaluate == 0)
          return true
      }
      case temp: RealNumber => {
        if (this.degree.evaluate == 0 && temp.evaluate == 0)
          return true
      }
      case temp: Indeterminate => {
        if (this.variable == temp.variable && this.degree == temp.degree)
          return true
      }
      case _ =>
    }
    false
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

  def apply(): Indeterminate = new Indeterminate(null, null, null, null)

}