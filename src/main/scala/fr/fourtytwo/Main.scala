package fr.fourtytwo

import fr.fourtytwo.exception._
import fr.fourtytwo.expression._
import fr.fourtytwo.token._

object Main {

  def main(args: Array[String]): Unit = {

    val expr = "X^2.0*4-X^1*-4+4=0"
    val expr1 = "4 * X^2 - 3 * X + 5 - X^2"
    val expr2 = "(y^2 * 4) + (4 * y^2) + (-4 * y^2) + (3 * y^3) + 3"
    val tokenizer = Tokenizer()

    try {
      val poly = Polynomial(expr)
      println("Expression: " + poly)
      println("Normalize:  " + poly.normExp)

    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
