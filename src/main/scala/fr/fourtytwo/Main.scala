package fr.fourtytwo

import fr.fourtytwo.exception._
import fr.fourtytwo.expression._
import fr.fourtytwo.polynomial.Polynomial
import fr.fourtytwo.polynomial.Polynomial._

object Main {

  def main(args: Array[String]): Unit = {

    val expr = "8 * y^0.12 * -4 * y^1 + y^1 + -4 * y^3 + 31 = 0"

    try {
      val polynomial = Polynomial(expr)

      println(s"Expected:   5 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 0")
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
