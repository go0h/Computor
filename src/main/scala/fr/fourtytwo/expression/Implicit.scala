package fr.fourtytwo.expression

object Implicit {

  implicit def doubleToRealNumber(a: Double): RealNumber = RealNumber(a)
  implicit def integerToRealNumber(a: Int): RealNumber = RealNumber(a.toDouble)

  implicit def stringToVariable(a: String): Variable = Variable(a)

}
