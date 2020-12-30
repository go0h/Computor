package fr.fourtytwo

import fr.fourtytwo.computor.{Computor, MATCHERS, TOKENIZER}
import fr.fourtytwo.exception.{EvaluateException, ParseException}
import fr.fourtytwo.expression.{ComplexNumber, Matrix, RealNumber, Variable}

object Main {

  def main(args: Array[String]): Unit = {

    try {

      val computor = new Computor
      computor.run()

    } catch {
      case ex: Exception => println(s"${ex.getClass.getName} ${ex.getMessage}")
    }
  }

}