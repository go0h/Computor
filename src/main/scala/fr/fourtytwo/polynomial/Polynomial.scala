package fr.fourtytwo.polynomial

import fr.fourtytwo.exception.EvaluateException
import fr.fourtytwo.expression._

class Polynomial(expr: String, debug: Boolean = false) {

  val expression: Expression = PolynomialReducer(expr)
  val exprArr: Array[Operable] = PolynomialReducer.exprToArray(expression)
    .map {
    case v: Variable => v.toIndeterminate
    case o: Operable => o
  }

  def solve(): (RealNumber, RealNumber) = {

    if (exprArr.length < 1)
      throw new EvaluateException(s"Can't solve: ${exprArr.mkString(" + ")} = 0")
    if (minDegree < 0)
      throw new EvaluateException(s"Can't solve polynomial with negative degree ${exprArr.mkString(" + ")} = 0")

    val maxSolveDegree = 2
    val degree = maxDegree

    println(s"Polynomial degree: $degree")
    if (degree == 0) {
      exprArr.head match {
        case r: RealNumber =>
          if (r.evaluate == 0) return (RealNumber(Double.NegativeInfinity), RealNumber(Double.PositiveInfinity))
          else throw new EvaluateException(s"Can't solve: ${exprArr.mkString(" + ")} = 0")
        case _ => throw new EvaluateException(s"Can't solve: ${exprArr.mkString(" + ")} = 0")
      }
    }
    if (degree > maxSolveDegree) {
      println(s"The polynomial degree is strictly greater than $maxSolveDegree ($degree), I can't solve")
      return null
    }
    val varName = getVarName

    val monomialNumbs = exprArr.length

    monomialNumbs match {
      case 1 => println(s"$varName = 0.0")
      case 2 =>
      case 3 =>
    }
    null
  }

  def maxDegree: Double = degree(_ > _)
  def minDegree: Double = degree(_ < _)
  def degree(order: (Double, Double) => Boolean): Double = {
    exprArr.map {
        case _: RealNumber => 0.0
        case _: Variable => 1.0
        case i: Indeterminate => i.degree.evaluate
      }.sortWith(order).head
  }

  override def toString: String = s"$expression = 0.0"

  def getVarName: String = {
    for (operable <- exprArr) {
      operable match {
        case o: Indeterminate => return o.variable.getName
        case _ =>
      }
    }
    null
  }
}

object Polynomial {

  def apply(expression: String): Polynomial = new Polynomial(expression)


}