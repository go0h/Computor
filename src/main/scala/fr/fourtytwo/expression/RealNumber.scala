package fr.fourtytwo.expression

class RealNumber(private val num: Double) extends Operable {

  def evaluate: Double = num
  def optimize: Expression = this
  override def toString: String = num.toString


  ////////////////////////////////////////
  ////////////// PLUS METHODS ////////////
  ////////////////////////////////////////
  def +(expr: RealNumber): Expression = RealNumber(num + expr.evaluate)

  def +(expr: Variable): Expression = {
    if (num == 0.0)
      return expr
    Operator(this, "+", expr)
  }

  def +(expr: Indeterminate): Expression = {
    if (num == 0.0)
      return expr
    Operator(this, "+", expr)
  }


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  def -(other: RealNumber): Expression = RealNumber(num - other.evaluate)
  def -(other: Variable): Expression = Operator(this, "-", other)
  def -(other: Indeterminate): Expression = Operator(this, "-", other)


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  def *(other: RealNumber): RealNumber = RealNumber(num * other.evaluate)

  def *(other: Variable): Expression = {
    if (num == 0)
      return RealNumber(0.0)
    new Indeterminate(num, other.toString, 1.0)
  }

  def *(other: Indeterminate): Expression = {
    if (num == 0)
      return RealNumber(0.0)
    new Indeterminate(num.toDouble * other.constant.evaluate,
      other.variable.toString,
      other.degree.evaluate)
  }


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  def /(other: RealNumber): RealNumber = {
    if (num == 0)
      return RealNumber(0.0)

    val denominator = other.evaluate
    if (denominator == 0)
      throw new ArithmeticException("Division by zero")
    RealNumber(num / denominator)
  }

  def /(other: Variable): Operable = {
    if (num == 0)
      return RealNumber(0.0)
    new Indeterminate(RealNumber(num), other, RealNumber(-1.0))
  }

  def /(other: Indeterminate): Operable = {
    if (num == 0)
      return RealNumber(0.0)
    new Indeterminate(RealNumber(num / other.constant.evaluate),
                      other.variable,
                      RealNumber(other.degree.evaluate * -1.0))
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case i: Int => num == i
      case d: Double => num == d
      case r: RealNumber => num == r.num
      case _: Throwable => false
    }
  }
}

object RealNumber {
  def apply(num: Double): RealNumber = new RealNumber(num)
}