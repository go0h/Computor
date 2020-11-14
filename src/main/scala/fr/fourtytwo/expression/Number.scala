package fr.fourtytwo.expression

class Number(private val num: Int) extends Expression {

  override def evaluate: Double = num

  def *(other: Number): Number = Number(num * other.evaluate.toInt)

  def *(other: RealNumber): RealNumber = RealNumber(num * other.evaluate)

  def *(other: Variable): Indeterminate = new Indeterminate(num, "*", other.toString, 1.0)

  def *(other: Indeterminate): Indeterminate = {
    new Indeterminate(num.toDouble * other.constant.evaluate,
      other.operator,
      other.variable.toString,
      other.degree.evaluate)
  }

  override def toString: String = num.toString
}

object Number {
  def apply(num: Int): Number = new Number(num)
}