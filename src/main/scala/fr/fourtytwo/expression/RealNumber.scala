package fr.fourtytwo.expression

import fr.fourtytwo.math.pow


class RealNumber(private val num: Double) extends Operable {

  def evaluate: Expression = this

  def getNum: Double = num

  def changeSign: RealNumber = RealNumber(num * -1)

  override def toString: String = num.toString
  ////////////////////////////////////////
  ////////////// PLUS METHODS ////////////
  ////////////////////////////////////////
  def +(other: Double): RealNumber = RealNumber(num + other)
  override def +(other: RealNumber): RealNumber = RealNumber(num + other.getNum)
  override def +(other: Variable): Expression = if (num == 0) other else Operator(this, "+", other)
  override def +(other: Indeterminate): Expression = if (num == 0) other else Operator(this, "+", other)
  override def +(other: ComplexNumber): Expression = other + this
  override def +(other: Matrix): Expression = throwException("+", other)


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  def -(other: Double): RealNumber = RealNumber(num - other)
  override def -(other: RealNumber): RealNumber = RealNumber(num - other.getNum)
  override def -(other: Variable): Expression = if (num == 0) other.changeSign else Operator(this, "-", other)
  override def -(other: Indeterminate): Operator = Operator(this, "-", other)
  override def -(other: ComplexNumber): Expression = {
    ComplexNumber(num - other.getRe, 0 - other.getIm).evaluate
  }
  override def -(other: Matrix): Expression = throwException("-", other)


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  def *(other: Int): RealNumber = RealNumber(num * other)
  def *(other: Double): RealNumber = RealNumber(num * other)
  override def *(other: RealNumber): RealNumber = RealNumber(num * other.getNum)
  override def *(other: Variable): Expression = other * this
  override def *(other: Indeterminate): Expression = other * this
  override def *(other: ComplexNumber): Expression = other * this
  override def *(other: Matrix): Expression = other * this


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  def /(other: Double): RealNumber = {
    if (other == 0)
      throw new ArithmeticException("Division by zero")
    RealNumber(num / other)
  }
  override def /(other: RealNumber): RealNumber = {
    if (other == 0)
      throw new ArithmeticException("Division by zero")
    RealNumber(num / other.getNum)
  }

  override def /(other: Variable): Operable = {
    if (num == 0)
      return RealNumber(0)
    new Indeterminate(num * other.getSign, other.getName, -1)
  }

  override def /(other: Indeterminate): Operable = {
    if (num == 0)
      return RealNumber(0)
    new Indeterminate(RealNumber(num / other.constant.getNum),
                      other.variable,
                      RealNumber(other.degree.getNum * -1))
  }

  override def /(other: ComplexNumber): Expression = ComplexNumber(num, 0) / other
  override def /(other: Matrix): Expression = throwException("/", other)

  ////////////////////////////////////////
  ///////////// POWER METHOD /////////////
  ////////////////////////////////////////
  def ^(other: Double): RealNumber = RealNumber(pow(num, other))
  override def ^(other: RealNumber): Expression = {
    RealNumber(pow(num, other.getNum))
  }



  ////////////////////////////////////////
  //////////// COMPARE METHOD ///////////
  ////////////////////////////////////////
  override def compare(other: Operable): Int = {
    other match {
      case rn : RealNumber => num.compare(rn.getNum)
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