package fr.fourtytwo

import fr.fourtytwo.exception._
import fr.fourtytwo.expression.{ComplexNumber, RealNumber, SIMPLE_TOKENIZER, Variable}
import fr.fourtytwo.polynomial.Polynomial

object Main {

  def main(args: Array[String]): Unit = {

    val expr = "4 + 732 * X^2 = 0"

    try {
      val polynomial = Polynomial(expr)
      val solution = polynomial.solve
      println(s"""The solutions is:
                  |$solution""".stripMargin)

    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
