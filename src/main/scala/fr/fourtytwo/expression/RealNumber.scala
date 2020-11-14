package fr.fourtytwo.expression

class RealNumber(private val num: Double) extends Expression {

  override def evaluate: Double = num

  override def toString: String = num.toString

  def *(other: Double): RealNumber = RealNumber(num * other)

  def *(other: RealNumber): RealNumber = RealNumber(num * other.evaluate)

  def *(other: Number): RealNumber = RealNumber(num * other.evaluate)

  def *(other: Variable): Indeterminate = new Indeterminate(num, "*", other.toString, 1.0)

  def *(other: Indeterminate): Indeterminate = {
    new Indeterminate(num.toDouble * other.constant.evaluate,
      other.operator,
      other.variable.toString,
      other.degree.evaluate)
  }
}

object RealNumber {
  def apply(num: Double): RealNumber = new RealNumber(num)
}