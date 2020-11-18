package fr.fourtytwo.expression

import fr.fourtytwo.exception.{EvaluateException, ParseException}

case class Indeterminate(constant: RealNumber,
                         variable: Variable,
                         degree: RealNumber) extends Operable {

  def this(c: Double, v: String, d: Double) {
    this(RealNumber(c), Variable(v), RealNumber(d))
  }

  override def evaluate: Double = {
    Operator(constant, "*", Operator(variable, "^", degree)).evaluate
  }
  override def optimize: Expression = {
    if (degree.evaluate == 0.0)
      return constant
    this
  }


  ////////////////////////////////////////
  //////////// ADDITION METHODS //////////
  ////////////////////////////////////////
  override def +(other: RealNumber): Expression = {
    if (other.evaluate == 0.0)
      return this
    Operator(this, "+", other)
  }

  override def +(other: Variable): Expression = {
    if (!variable.equals(other))
      throw new EvaluateException(s"Can't add different variables ($variable, $other)")
    if (degree.evaluate == 1.0)
      return Indeterminate(RealNumber(constant.evaluate + 1.0), other, degree)
    Operator(this, "+", other)
  }

  override def +(other: Indeterminate): Expression = {
    if (!variable.equals(other.variable))
      throw new EvaluateException(s"Can't add different variables ($variable, ${other.variable})")
    if (degree.evaluate == other.degree.evaluate)
      return Indeterminate(RealNumber(constant.evaluate + other.constant.evaluate),
                           variable,
                           degree)
    Operator(this, "+", other)
  }


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  override def -(other: RealNumber): Expression = {
    if (other.evaluate == 0)
      return this
    Operator(this, "-", other)
  }

  override def -(other: Variable): Expression = {
    if (!variable.equals(other))
      throw new EvaluateException(s"Can't sub different variables ($variable, $other)")
    if (degree.evaluate == 1) {
      if (constant.evaluate == 1)
        return RealNumber(0.0)
      return Indeterminate(RealNumber(this.constant.evaluate - 1), variable, degree)
    }
    Operator(this, "-", other)
  }

  override def -(other: Indeterminate): Expression = {
    if (!variable.equals(other.variable))
      throw new EvaluateException(s"Can't sub different variables ($variable, ${other.variable})")
    if (degree.evaluate == other.degree.evaluate) {
      return Indeterminate(RealNumber(constant.evaluate - other.constant.evaluate),
                          variable,
                          degree)
    }
    Operator(this, "-", other)
  }


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  def *(other: RealNumber): Expression = {
    new Indeterminate(constant * other, variable, degree)
  }

  def *(other: Variable): Expression = {
    if (!variable.equals(other))
      throw new EvaluateException(s"Can't multiply different variables ($variable, $other)")

    new Indeterminate(this.constant, variable, RealNumber(degree.evaluate * 2))
  }

  def *(other: Indeterminate): Expression = {
    if (!variable.equals(other.variable))
      throw new EvaluateException(s"Can't multiply different variables ($variable, ${other.variable})")

    new Indeterminate(constant * other.constant,
                      variable,
                      RealNumber(degree.evaluate + other.degree.evaluate))
  }


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
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
    val res = new Indeterminate(constant / other.constant,
                                variable,
                                RealNumber(degree.evaluate - other.degree.evaluate))

    if (res.degree.equals(RealNumber(0)))
      return res.constant
    res
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

    val prefixPattern = """^(-?\d+(?:\.\d+)?)([*])([A-Za-z]+)\^(-?\d+(?:\.\d+)?)$""".r
    val suffixPattern = """(^[A-Za-z]+)\^([0-9]+(?:\.\d+)?)([*])(-?[0-9]+(?:\.\d+)?)$""".r

    expression match {
      case prefixPattern(c, _, n, d) => new Indeterminate(c.toDouble, n, d.toDouble)
      case suffixPattern(n, d, _, c) => new Indeterminate(c.toDouble, n, d.toDouble)
      case _ => throw new ParseException(s"Indeterminate $expression is not well formatted")
    }
  }

  def apply(): Indeterminate = new Indeterminate(null, null, null)

}