package fr.fourtytwo.expression

import fr.fourtytwo.exception.{EvaluateException, ParseException}

case class Indeterminate(constant: RealNumber,
                         variable: Variable,
                         degree: RealNumber) extends Expression {

  def this(c: Double, v: String, d: Double) {
    this(RealNumber(c), Variable(v), RealNumber(d))
  }

  override def evaluate: Double = {
    Operator(constant, "*", Operator(variable, "^", degree)).evaluate
  }

  def *(other: RealNumber): Indeterminate = {
    new Indeterminate(constant * other, variable, degree)
  }

  def *(other: Variable): Indeterminate = {

    if (!variable.equals(other))
      throw new EvaluateException(s"Can't multiply different variables ($variable, $other)")

    new Indeterminate(this.constant, variable, RealNumber(degree.evaluate * 2))
  }

  def *(other: Indeterminate): Indeterminate = {
    if (!variable.equals(other.variable))
      throw new EvaluateException(s"Can't multiply different variables ($variable, ${other.variable})")
    new Indeterminate(constant * other.constant,
                      variable,
                      RealNumber(degree.evaluate + other.degree.evaluate))
  }

  def /(other: RealNumber): Indeterminate = {
    new Indeterminate(constant / other, variable, degree)
  }

  def /(other: Variable): Expression = {
    if (!variable.equals(other))
      throw new EvaluateException(s"Can't division different variables ($variable, $other)")
    if (degree.evaluate == 1)
      return constant
    new Indeterminate(constant, variable, RealNumber(degree.evaluate - 1.0))
  }

  def /(other: Indeterminate): Expression = {
    if (!variable.equals(other.variable))
      throw new EvaluateException(s"Can't division different variables ($variable, ${other.variable})")

    if (equals(other))
      return RealNumber(1.0)
    new Indeterminate(constant / other.constant,
                      variable,
                      RealNumber(degree.evaluate - other.degree.evaluate))
  }

  override def toString: String = s"($constant * $variable^$degree)"

  override def equals(other: Any): Boolean = {

    other match {
      case temp: RealNumber => {
        if (this.degree.evaluate == 0 && temp.evaluate == 0)
          return true
      }
      case temp: Variable => {
        if (variable.equals(temp) && degree.evaluate == 1.0 && constant.evaluate == 1.0)
          return true
      }
      case temp: Indeterminate => {
        if (variable.equals(temp.variable) && degree.equals(temp.degree) && constant.equals(temp.constant))
          return true
      }
      case _ =>
    }
    false
  }
}

object Indeterminate {

  def apply(expression: String): Indeterminate = {

    val prefixPattern = """^(-?\d+(?:\.\d+)?)([*])([A-Za-z]+)\^(\d+(?:\.\d+)?)$""".r
    val suffixPattern = """(^[A-Za-z]+)\^([0-9]+(?:\.\d+)?)([*])(-?[0-9]+(?:\.\d+)?)$""".r

    expression match {
      case prefixPattern(c, op, n, d) => new Indeterminate(c.toDouble, n, d.toDouble)
      case suffixPattern(n, d, op, c) => new Indeterminate(c.toDouble, n, d.toDouble)
      case _ => throw new ParseException(s"Indeterminate $expression is not well formatted")
    }
  }

  def apply(): Indeterminate = new Indeterminate(null, null, null)

}