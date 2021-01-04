package fr.fourtytwo.expression

import fr.fourtytwo.math.pow
import fr.fourtytwo.exception._


case class Indeterminate(constant: RealNumber,
                         variable: Variable,
                         degree: RealNumber) extends Operable {

  def this(c: Double, v: String, d: Double) = {
    this(RealNumber(c), Variable(v), RealNumber(d))
  }

  def evaluate: Expression = {
    if (constant == 0)
      return RealNumber(0)
    if (degree == 0)
      return constant
    if (variable.getSign == -1)
      return Indeterminate(constant.changeSign, variable.changeSign, degree)
    this
  }

  def changeSign: Indeterminate = Indeterminate(constant.changeSign, variable, degree)

  ////////////////////////////////////////
  //////////// ADDITION METHODS //////////
  ////////////////////////////////////////
  override def +(other: RealNumber): Expression = {
    if (other == 0)
      return this
    Operator(this, "+", other)
  }

  override def +(other: Variable): Expression = {
    if (!variable.getName.equalsIgnoreCase(other.getName))
      return Operator(this, "+", other)
    if (degree == 1)
      return Indeterminate(constant + other.getSign, Variable(other.getName), degree)
    Operator(this, "+", other)
  }

  override def +(other: Indeterminate): Expression = {
    if (variable.equals(other.variable) && degree.equals(other.degree))
      return Indeterminate(constant + other.constant, variable, degree)
    Operator(this, "+", other)
  }
  override def +(other: ComplexNumber): Expression = Operator(this, "+", other)
  override def +(other: Matrix): Expression = throwException("+", other)


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  override def -(other: RealNumber): Expression = {
    if (other == 0)
      return this
    Operator(this, "-", other)
  }

  override def -(other: Variable): Expression = {
    if (!variable.getName.equalsIgnoreCase(other.getName))
      return Operator(this, "-", other)
    if (degree == 1) {
      if (constant == 1)
        return RealNumber(1 - other.getSign)
      return Indeterminate(constant - other.getSign, variable, degree)
    }
    Operator(this, "-", other)
  }

  override def -(other: Indeterminate): Expression = {
    if (!variable.equals(other.variable))
      return Operator(this, "-", other)
    if (degree.equals(other.degree)) {
      return Indeterminate(constant - other.constant, variable, degree)
    }
    Operator(this, "-", other)
  }
  override def -(other: ComplexNumber): Expression = Operator(this, "-", other)
  override def -(other: Matrix): Expression = throwException("-", other)

  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  override def *(other: RealNumber): Expression = {
    if (other == 0)
      return RealNumber(0)
    Indeterminate(constant * other, variable, degree)
  }

  override def *(other: Variable): Expression = {
    if (!variable.getName.equalsIgnoreCase(other.getName))
      return Operator(this, "*", other)
    Indeterminate(constant * other.getSign, Variable(other.getName), degree + 1)
  }

  override def *(other: Indeterminate): Expression = {
    if (!variable.equals(other.variable))
      return Operator(this, "*", other)
    Indeterminate(constant * other.constant, variable, degree + other.degree)
  }
  override def *(other: ComplexNumber): Expression = Operator(this, "*", other)


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  override def /(other: RealNumber): Indeterminate = {
    new Indeterminate(constant / other, variable, degree)
  }

  override def /(other: Variable): Expression = {
    if (!variable.getName.equalsIgnoreCase(other.getName))
      return Operator(this, "/", other)
    if (degree == 1)
      return constant * other.getSign
    Indeterminate(constant * other.getSign, Variable(other.getName), degree - 1)
  }

  override def /(other: Indeterminate): Expression = {
    if (!variable.equals(other.variable))
      return Operator(this, "/", other)

    if (equals(other))
      return RealNumber(1)
    val res = new Indeterminate(constant / other.constant, variable, degree - other.degree)

    if (res.degree == 0)
      return res.constant
    res
  }
  override def /(other: ComplexNumber): Expression = Operator(this, "/", other)

  ////////////////////////////////////////
  ///////////// POWER METHOD /////////////
  ////////////////////////////////////////
  override def ^(other: RealNumber): Expression = {
    if (other == 0)
      return constant
    if (other == 1)
      return this
    Indeterminate(RealNumber(pow(constant.getNum, other.getNum)), variable, degree * other)
  }


  ////////////////////////////////////////
  //////////// COMPARE METHOD ////////////
  ////////////////////////////////////////
  def compare(other: Operable): Int = {
    other match {
      case _ : RealNumber => -1
      case _ : ComplexNumber => -1
      case _ : Variable => {
        if (constant == 1 && degree == 1)
          return 0
        if (degree.getNum < 1)
          return 1
        -1
      }
      case i : Indeterminate => {
        if (degree.getNum < i.degree.getNum)
          return 1
        if (degree.equals(i.degree))
          return constant.getNum.compare(i.constant.getNum)
        -1
      }
      case _ : Matrix => 1
    }
  }

  override def toString: String = {
    if (constant == 1) {
      if (degree == 1)
        return variable.toString
      return s"$variable^$degree"
    }
    if (degree == 1)
      return s"$constant * $variable"
    s"$constant * $variable^$degree"
  }

  override def equals(other: Any): Boolean = {

    other match {
      case r: RealNumber =>
        if (degree == 0 && r == 0)
          return true
      case v: Variable =>
        if (variable.equals(v) && degree == 1 && constant == 1)
          return true
      case ind: Indeterminate =>
        if (variable.equals(ind.variable) && degree.equals(ind.degree) && constant.equals(ind.constant))
          return true
      case _ =>
    }
    false
  }
  def contains(op: String): Boolean = false
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