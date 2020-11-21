package fr.fourtytwo

import fr.fourtytwo.exception._
import fr.fourtytwo.expression._
import fr.fourtytwo.polynomial.Polynomial
import fr.fourtytwo.polynomial.Polynomial._

object Main {


  //"8 * y^0.12 * -4 * y^1 + y^1 + -4 * y^3 + 31"
  //((((-32.0 * y^1.12) + (1.0 * y^1.0)) + (-4.0 * y^3.0)) + 31.0)

  def main(args: Array[String]): Unit = {

    val expr = "X^2 - X^2 + X + 13 * 2 = 0"

    try {
      val polynomial = Polynomial(expr)

      println("Expected:   (1.0 * X^1.0) + 26.0 = 0")
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
