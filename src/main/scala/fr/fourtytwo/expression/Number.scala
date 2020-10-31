package fr.fourtytwo.expression

class Number(private val num: Int) extends Expression {

  override def evaluate: Double = num

  override def toString: String = num.toString
}

object Number {
  def apply(num: Int): Number = new Number(num)

  def unapply(arg: String): Option[Int] = {
    try {
      Some(arg.toInt)
    } catch {
      case _ => None
    }
  }
}