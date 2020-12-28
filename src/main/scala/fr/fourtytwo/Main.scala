package fr.fourtytwo

import fr.fourtytwo.computor._
import fr.fourtytwo.exception._

import scala.io.{Source, StdIn}


object Main {

  def main(args: Array[String]): Unit = {

    var line = StdIn.readLine()
    while (line != null) {
      if (line.equals("q") || line.equals("quit"))
        line = null
      else {
        println(s"$line|")
        line = StdIn.readLine()
      }
    }

    val expr: String = "pow(-2, -3)"

    try {

      val tokens = SIMPLE_TOKENIZER.generateTokens(expr)

      val rpn = Computor(tokens).solve
      val result = rpn.evaluate
      println(s"The result is: $result = ${scala.math.pow(-2, -3)}")

    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case aEx: ArithmeticException => println(aEx.getMessage)
      case ex: Exception => println(ex.getClass.toString + " " + ex.getMessage)
    }
  }
}