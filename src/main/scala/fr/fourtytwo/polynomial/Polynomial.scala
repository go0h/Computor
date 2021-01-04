package fr.fourtytwo.polynomial

import scala.sys.exit
import java.util.logging.{Level, Logger}
import fr.fourtytwo.exception.{EvaluateException, ParseException}
import fr.fourtytwo.math._
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
    val a = exprArr.head.asInstanceOf[Indeterminate].constant.getNum
    var b = 0.0
    var c = 0.0

    exprArr match {
      case Array(_, b1, c1) => {
        b = b1.asInstanceOf[Indeterminate].constant.getNum
        c = c1.asInstanceOf[RealNumber].getNum
      }
      case Array(_, b1) => {
        b1 match {
          case i: Indeterminate => b = i.constant.getNum
          case r: RealNumber => c = r.getNum
        }
      }
      case Array(_) =>
    }

    val discriminant = (b * b) - 4 * a * c
    LOGGER.fine(s"Discriminant = $b^2 - 4 * $a * $c = $discriminant")

    if (discriminant < 0) {
      LOGGER.info("Discriminant is strictly negative, the two solutions are:")
      val compDisc = ComplexNumber(0, sqrt(-discriminant))
      val x1 = (RealNumber(-b) + compDisc).asInstanceOf[Operable] / RealNumber(2 * a)
      val x2 = (RealNumber(-b) - compDisc).asInstanceOf[Operable] / RealNumber(2 * a)

      s"""x1 = $x1
         |x2 = $x2""".stripMargin
    }
    else if (discriminant == 0) {
      LOGGER.info("Discriminant is equal zero, the one solution is:")
      val res = (-b) / (2 * a)
      s"x = ${if (res == 0) 0.0 else res}"
    }
    else {
      LOGGER.info("Discriminant is strictly positive, the two solutions are:")
      val x1 = ((-b) + sqrt(discriminant)) / (2 * a)
      val x2 = ((-b) - sqrt(discriminant)) / (2 * a)

      s"""x1 = $x1
         |x2 = $x2""".stripMargin
    }
  }

  def monomialSolve: String = {
    val a = exprArr.head.asInstanceOf[Indeterminate].constant.getNum
    var b = 0.0

    exprArr match {
      case Array(_, b1) => b = b1.asInstanceOf[RealNumber].getNum
      case Array(_) =>
    }
    val res = (-b) / a
    s"x = ${if (res == 0) 0.0 else res}"
  }

  def noVars: String = {
    val a = exprArr.head.asInstanceOf[RealNumber].getNum
    if (a != 0)
      throw new EvaluateException(s"The polynomial $toString is wrong. There is no solution")
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
        case i: Indeterminate => i.degree.getNum
      }.sortWith(_ > _)
  }

  override def toString: String = s"$expression = 0.0"

  def numVars: Int = 1
  def apply(args: Expression*): Expression = ???
}

object Polynomial {

  def apply(expression: String, debug: Boolean = false): Polynomial = {
    new Polynomial(expression, debug = debug)
  }

  def main(args: Array[String]): Unit = {

    var debug: Boolean = false

    if (args.contains("-h") || args.contains("--help") || args.isEmpty || args.length > 2) {
      println(USAGE)
      exit(0)
    }
    else if (args.length == 2 && (args(1).equals("-d") || args(1).equals("--debug"))) {
      debug = true
    }
    else if (args.length == 2) {
      println(USAGE)
      exit(0)
    }

    val expr: String = args(0)

    try {
      val polynomial = Polynomial(expr, debug)
      val solution = polynomial.solve
      println(s"""The solutions is:
                 |$solution""".stripMargin)
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass.toString + " " + ex.getMessage)
    }
  }
}