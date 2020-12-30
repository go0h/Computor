package fr.fourtytwo

import fr.fourtytwo.computor.{Computor, MATCHERS, SIMPLE_TOKENIZER}
import fr.fourtytwo.exception.{EvaluateException, ParseException}
import fr.fourtytwo.expression.Variable

object Main {

  def main(args: Array[String]): Unit = {

      try {

        val res = Variable("X") + Variable("Y")

        println(res, res.getType)

      } catch {
        case ex: ParseException => println(ex.getMessage)
        case ex: EvaluateException => println(ex.getMessage)
        case ex: Exception => println(s"${ex.getClass.getName} ${ex.getMessage}")
      }

//    val computor = new Computor
//    computor.run()
  }

}