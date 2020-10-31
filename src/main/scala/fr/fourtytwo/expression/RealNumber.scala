package fr.fourtytwo.expression

class RealNumber(private val num: Double) extends Expression {

  override def evaluate: Double = num

  override def toString: String = num.toString
}

object RealNumber {
  def apply(num: Double): RealNumber = new RealNumber(num)

  def unapply(arg: String): Option[Double] = {
    try {
      Some(arg.toDouble)
    } catch {
      case _ => None
    }
  }
}