package fr.fourtytwo

import fr.fourtytwo.exception._
import fr.fourtytwo.expression.SIMPLE_TOKENIZER
import fr.fourtytwo.polynomial.Polynomial

object Main {

  def main(args: Array[String]): Unit = {

    val expr = "-( (5 * X^1) - ( (4 * X^1 + 4) + 4) ) = 0"
    val rpnTest = "-(7845 + 32 -3 * 323.4)"


    try {
      var res = RPN(SIMPLE_TOKENIZER.generateTokens(rpnTest)).solve



      val polynomial = Polynomial(expr)
      val solution = polynomial.solve()
//      println(s"[${solution._1}, ${solution._2}]")

    } catch {
      case ex: ParseException => println(s"Parse: ${ex.getMessage}")
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
