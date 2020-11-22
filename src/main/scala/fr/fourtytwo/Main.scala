package fr.fourtytwo

import fr.fourtytwo.exception._
import fr.fourtytwo.polynomial.Polynomial

object Main {

  def main(args: Array[String]): Unit = {

    val expr = "0 = X^2 - X^2 + X + 13 * 2"

    try {
      val polynomial = Polynomial(expr)
      println(s"Expected:   -X + -26.0 = 0.0")
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
