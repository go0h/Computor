package fr.fourtytwo.expression

class Number(private val num: Int) extends Expression {

  override def evaluate: Double = num

  override def toString: String = num.toString
}

object Number {
  def apply(num: Int): Number = {
    new Number(num)
  }
}