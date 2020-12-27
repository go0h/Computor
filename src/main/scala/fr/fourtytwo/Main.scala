package fr.fourtytwo

import fr.fourtytwo.computor._
import fr.fourtytwo.exception._


object Main {

  def main(args: Array[String]): Unit = {

    val expr: String = "pow(-2, -3)"

    try {

      val tokens = SIMPLE_TOKENIZER.generateTokens(expr)
      println(s"expr = ${tokens.map(_.expr).mkString("")}")

      val rpn = Computor(tokens).solve
      val result = rpn.evaluate
      println(s"The result is: $result")
      println(scala.math.pow(-2, -3))

    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case aEx: ArithmeticException => println(aEx.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}