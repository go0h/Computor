package fr.fourtytwo

import fr.fourtytwo.exception.ParseException
import fr.fourtytwo.token._

object Main {

  def main(args: Array[String]): Unit = {


    TokenType.values.toArray.map(x => (x, x.toString.r)).toMap

    val expr = "5.0007*X^0.00+(4*X)^1-9.3*X^2=1*X^0"
    val tokenizer = Tokenizer()

    try {
      val tokens = tokenizer.generateTokens(expr)
      tokens.foreach(x => println(x.expr + "\t\t" + x.tokenType))
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex1: Exception => println("Exception " + ex1.getMessage)
    }
  }
}
