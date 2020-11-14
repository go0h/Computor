package fr.fourtytwo.expression

import fr.fourtytwo.exception.{EvaluateException, ParseException}

case class Indeterminate(constant: RealNumber,
                    operator: String,
                    variable: Variable,
                    degree: RealNumber) extends Expression {

  def this(c: Double, o: String, v: String, d: Double) {
    this(RealNumber(c), o, Variable(v), RealNumber(d))
  }

  override def evaluate: Double = {
    Operator(constant, operator, Operator(variable, "^", degree)).evaluate
  }

  def *(other: Number): Indeterminate = {
    new Indeterminate(constant * other, operator = "*", variable, degree)
  }

  def *(other: RealNumber): Indeterminate = {
    new Indeterminate(constant * other, operator = "*", variable, degree)
  }

  def *(other: Variable): Indeterminate = {

    if (!variable.equals(other))
      throw new EvaluateException(s"Can't multiply different variables ($variable, $other)")

    new Indeterminate(this.constant, operator = "*", variable, RealNumber(degree.evaluate * 2))
  }

  def *(other: Indeterminate): Indeterminate = {

    if (!variable.equals(other.variable))
      throw new EvaluateException(s"Can't multiply different variables ($variable, ${other.variable})")

    new Indeterminate(
      this.constant * other.constant,
      operator = "*",
      variable,
      RealNumber(degree.evaluate + other.degree.evaluate))
  }

  override def toString: String = s"($constant $operator $variable^$degree)"

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

    val prefixPattern = """^(-?\d+(?:\.\d+)?)([*\\])([A-Za-z]+)\^(\d+(?:\.\d+)?)$""".r
    val suffixPattern = """(^[A-Za-z]+)\^([0-9]+(?:\.\d+)?)([*\\])(-?[0-9]+(?:\.\d+)?)$""".r

    expression match {
      case prefixPattern(c, op, n, d) => new Indeterminate(c.toDouble, op, n, d.toDouble)
      case suffixPattern(n, d, op, c) => new Indeterminate(c.toDouble, op, n, d.toDouble)
      case _ => throw new ParseException(s"Indeterminate $expression is not well formatted")
    }
  }

  def apply(): Indeterminate = new Indeterminate(null, null, null, null)

}