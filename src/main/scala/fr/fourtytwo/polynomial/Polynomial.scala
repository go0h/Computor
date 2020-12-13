package fr.fourtytwo.polynomial

import scala.math.sqrt
import java.util.logging.{Level, Logger}
import fr.fourtytwo.exception.EvaluateException
import fr.fourtytwo.expression._
import fr.fourtytwo.utils.ComputorLogger


class Polynomial(expr: String, debug: Boolean = false) {

  val LOGGER: Logger = ComputorLogger.LOGGER
  LOGGER.setLevel(if (debug) Level.FINE else Level.INFO)

  val expression: Expression = PolynomialReducer(expr)
  val exprArr: Array[Operable] = PolynomialReducer.exprToArray(expression)
    .map {
    case v: Variable => v.toIndeterminate
    case o: Operable => o
  }

  def solve: String = {

    val degrees = getDegrees

    LOGGER.info(s"Polynomial degree: ${degrees.max}")
    checkSolvable(degrees)

    LOGGER.fine(s"Degrees = ${degrees.mkString(", ")}")
    degrees.max.toInt match {
      case 2 => binomialSolve
      case 1 => monomialSolve
      case 0 => noVars
      case _ => s"Can't recognize polynomial $toString"
    }
  }

  def binomialSolve: String = {
    val a = exprArr.head.asInstanceOf[Indeterminate].constant.evaluate
    var b = 0.0
    var c = 0.0

    exprArr match {
      case Array(_, b1, c1) => {
        b = b1.asInstanceOf[Indeterminate].constant.evaluate
        c = c1.asInstanceOf[RealNumber].evaluate
      }
      case Array(_, b1) => {
        b1 match {
          case i: Indeterminate => b = i.constant.evaluate
          case r: RealNumber => c = r.evaluate
        }
      }
      case Array(_) =>
    }

    val discriminant = (b * b) - 4 * a * c
    LOGGER.fine(s"Discriminant = $b^2 - 4 * $a * $c = $discriminant")

    if (discriminant < 0) {
      val compDisc = ComplexNumber(0, sqrt(-discriminant))
      val x1 = (RealNumber(-b) + compDisc).asInstanceOf[Operable] / RealNumber(2 * a)
      val x2 = (RealNumber(-b) - compDisc).asInstanceOf[Operable] / RealNumber(2 * a)

      s"""x1 = $x1
         |x2 = $x2""".stripMargin
    }
    else if (discriminant == 0) {
      val res = (-b) / (2 * a)
      s"x = ${if (res == 0) 0.0 else res}"
    }
    else {
      val x1 = ((-b) + sqrt(discriminant)) / (2 * a)
      val x2 = ((-b) - sqrt(discriminant)) / (2 * a)

      s"""x1 = $x1
         |x2 = $x2""".stripMargin
    }
  }

  def monomialSolve: String = {
    val a = exprArr.head.asInstanceOf[Indeterminate].constant.evaluate
    var b = 0.0

    exprArr match {
      case Array(_, b1) => b = b1.asInstanceOf[RealNumber].evaluate
      case Array(_) =>
    }
    val res = (-b) / a
    s"x = ${if (res == 0) 0.0 else res}"
  }

  def noVars: String = {
    val a = exprArr.head.asInstanceOf[RealNumber].evaluate
    if (a != 0)
      throw new EvaluateException(s"The polynomial $toString is wrong")
    "All real numbers"
  }

  def checkSolvable(degrees: Array[Double]): Unit = {

    val maxSolveDegree = 2

    if (degrees.length < 1)
      throw new EvaluateException(s"Can't solve empty expression")

    if (degrees.length > 3)
      throw new EvaluateException(s"Can't solve expression $toString")

    if (degrees.min < 0)
      throw new EvaluateException(s"Can't solve polynomial with negative degree $toString = 0")

    if (degrees.max > maxSolveDegree)
      throw new EvaluateException(s"The polynomial degree is strictly greater than $maxSolveDegree" +
        s"(${degrees.max}), I can't solve")

    for (degree <- degrees) {
      if (degree - degree.toInt != 0.0)
        throw new EvaluateException(s"The polynomial degree isn't Integer $degree, I can't solve")
    }
  }

  def getDegrees: Array[Double] = {
    exprArr.map {
        case _: RealNumber => 0.0
        case _: Variable => 1.0
        case i: Indeterminate => i.degree.evaluate
      }.sortWith(_ > _)
  }

  override def toString: String = s"$expression = 0.0"
}

object Polynomial {
  def apply(expression: String, debug: Boolean = false): Polynomial = {
    new Polynomial(expression, debug)
  }
}