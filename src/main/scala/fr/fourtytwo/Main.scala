package fr.fourtytwo

import fr.fourtytwo.computor.Computor.basicCheck
import fr.fourtytwo.computor._
import fr.fourtytwo.exception.{EvaluateException, ParseException}
import fr.fourtytwo.expression.{Indeterminate, Matrix, RealNumber, Variable}

object Main {

  def main(args: Array[String]): Unit = {

      try {

        val expr1 = "[ [1, 2, 3]; [4, 5, 6] ]"
        val expr2 = "[ [2.5]; [2.5]; [5] ]"
        val res =   "[ [22.5]; [52.5] ]"

        val m1 = Matrix(expr1)
        val m2 = Matrix(expr2)


        println(m1 * m2)

      } catch {
        case ex: ParseException => println(ex.getMessage)
        case ex: EvaluateException => println(ex.getMessage)
        case ex: Exception => println(s"${ex.getClass.getName} ${ex.getMessage}")
      }

//    val computor = new Computor
//    computor.run()
  }

}