package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException

import scala.math.pow

class RealNumber(private val num: Double) extends Operable {

  def evaluate: Double = num
  def simplify: Expression = this
  override def toString: String = num.toString


  def changeSign: RealNumber = RealNumber(num * -1)

  ////////////////////////////////////////
  ////////////// PLUS METHODS ////////////
  ////////////////////////////////////////
  def +(other: RealNumber): Expression = RealNumber(num + other.evaluate)
  def +(other: Variable): Expression = if (num == 0) other else Operator(this, "+", other)
  def +(other: Indeterminate): Expression = if (num == 0) other else Operator(this, "+", other)


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  def -(other: RealNumber): Expression = RealNumber(num - other.evaluate)
  def -(other: Variable): Expression = if (num == 0) other.changeSign else Operator(this, "-", other)
  def -(other: Indeterminate): Expression = Operator(this, "-", other)


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  def *(other: RealNumber): RealNumber = RealNumber(num * other.evaluate)

  def *(other: Variable): Expression = {
    if (num == 0)
      return RealNumber(0)
    new Indeterminate(num * other.getSign, other.getName, 1)
  }

  def *(other: Indeterminate): Expression = {
    if (num == 0)
      return RealNumber(0)
    new Indeterminate(num.toDouble * other.constant.evaluate,
      other.variable.toString,
      other.degree.evaluate)
  }


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  def /(other: RealNumber): RealNumber = {
    if (num == 0)
      return RealNumber(0)

    val denominator = other.evaluate
    if (denominator == 0)
      throw new ArithmeticException("Division by zero")
    RealNumber(num / denominator)
  }

  def /(other: Variable): Operable = {
    if (num == 0)
      return RealNumber(0)
    new Indeterminate(num * other.getSign, other.getName, -1)
  }

  def /(other: Indeterminate): Operable = {
    if (num == 0)
      return RealNumber(0)
    new Indeterminate(RealNumber(num / other.constant.evaluate),
                      other.variable,
                      RealNumber(other.degree.evaluate * -1))
  }

  ////////////////////////////////////////
  ///////////// POWER METHOD /////////////
  ////////////////////////////////////////
  def ^(other: RealNumber): Expression = RealNumber(pow(num, other.evaluate))



  ////////////////////////////////////////
  //////////// COMPARE METHOD ///////////
  ////////////////////////////////////////
  override def compare(other: Operable): Int = {
    other match {
      case rn : RealNumber => num.compare(rn.evaluate)
      case _ : Variable => 1
      case _ : Indeterminate => 1
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
  def contains(op: String): Boolean = false
}

object RealNumber {
  def apply(num: Double): RealNumber = new RealNumber(num)
}