package fr.fourtytwo

import fr.fourtytwo.expression._
import fr.fourtytwo.parser.Parser
import fr.fourtytwo.exception.ParseException

object Main {

  def main(args: Array[String]): Unit = {

    val test = Operator(Number(7), "*", Number(3))
    println(s"$test = ${test.evaluate}")

    try {
      val temp = new Parser("((5))+4 * X + (X^2= X^2")
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex1: Exception => println("Exception " + ex1.getMessage)
    }
  }
}
