package fr.fourtytwo.expression

import scala.math.pow


class RealNumber(private val num: Double) extends Operable {

  def evaluate: Double = num
  def simplify: Expression = this
  override def toString: String = num.toString


  def changeSign: RealNumber = RealNumber(num * -1)

  ////////////////////////////////////////
  ////////////// PLUS METHODS ////////////
  ////////////////////////////////////////
  override def +(other: RealNumber): Expression = RealNumber(num + other.evaluate)
  override def +(other: Variable): Expression = if (num == 0) other else Operator(this, "+", other)
  override def +(other: Indeterminate): Expression = if (num == 0) other else Operator(this, "+", other)
  override def +(other: ComplexNumber): Expression = other + this


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  override def -(other: RealNumber): Expression = RealNumber(num - other.evaluate)
  override def -(other: Variable): Expression = if (num == 0) other.changeSign else Operator(this, "-", other)
  override def -(other: Indeterminate): Expression = Operator(this, "-", other)
  override def -(other: ComplexNumber): Expression = {
    ComplexNumber(num - other.getRe, 0 - other.getIm).simplify
  }


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  override def *(other: RealNumber): RealNumber = RealNumber(num * other.evaluate)

  override def *(other: Variable): Expression = {
    if (num == 0)
      return RealNumber(0)
    new Indeterminate(num * other.getSign, other.getName, 1)
  }

  override def *(other: Indeterminate): Expression = {
    if (num == 0)
      return RealNumber(0)
    new Indeterminate(num.toDouble * other.constant.evaluate,
      other.variable.toString,
      other.degree.evaluate)
  }
  override def *(other: ComplexNumber): Expression = other * this
  override def *(other: Matrix): Expression = other * this


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  override def /(other: RealNumber): RealNumber = {
    if (other.evaluate == 0)
      throw new ArithmeticException("Division by zero")
    if (num == 0)
      return RealNumber(0)
    RealNumber(num / other.evaluate)
  }

  override def /(other: Variable): Operable = {
    if (num == 0)
      return RealNumber(0)
    new Indeterminate(num * other.getSign, other.getName, -1)
  }

  override def /(other: Indeterminate): Operable = {
    if (num == 0)
      return RealNumber(0)
    new Indeterminate(RealNumber(num / other.constant.evaluate),
                      other.variable,
                      RealNumber(other.degree.evaluate * -1))
  }

  override def /(other: ComplexNumber): Expression = ComplexNumber(num, 0) / other

  ////////////////////////////////////////
  ///////////// POWER METHOD /////////////
  ////////////////////////////////////////
  override def ^(other: RealNumber): Expression = {
    RealNumber(pow(num, other.evaluate))
  }



  ////////////////////////////////////////
  //////////// COMPARE METHOD ///////////
  ////////////////////////////////////////
  override def compare(other: Operable): Int = {
    other match {
      case rn : RealNumber => num.compare(rn.evaluate)
      case _ : Variable => 1
      case _ : Indeterminate => 1
      case _ : ComplexNumber => 1
      case _ : Matrix => 1
    }
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case i: Int => num == i
      case d: Double => num == d
      case r: RealNumber => num == r.num
      case _ => false
    }
  }
  def ==(obj: Int): Boolean = equals(obj)
  def ==(obj: Double): Boolean = equals(obj)
  def contains(op: String): Boolean = false
}

object RealNumber {
  def apply(num: Double): RealNumber = new RealNumber(num)
}