package fr.fourtytwo.expression

class Number(private val num: Double) extends Expression {

  override def evaluate: Double = num

  override def toString: String = num.toString
}

object Number {
  def apply(num: Double): Number = new Number(num)
}