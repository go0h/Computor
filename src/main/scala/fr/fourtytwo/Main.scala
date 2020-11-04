package fr.fourtytwo

import fr.fourtytwo.exception._
import fr.fourtytwo.token._

import scala.math.pow

object Main {

  def main(args: Array[String]): Unit = {

    TokenType.values.toArray.map(x => (x, x.toString.r)).toMap

    val expr = "(3.23144 + 32 % 23 * 765.321)^(-3/(0.4)) / (-0.0321) + 16.308"
    val tokenizer = Tokenizer()

    try {

      val tokens = tokenizer.generateTokens(expr)
      val rpn = RPN(tokens)
      println(rpn.getTokens.map(x => x.expr).mkString(" ") + " = " + rpn.solve)
      println("RIGHT = " + (pow(3.23144 + 32 % 23 * 765.321,-3/0.4) / (-0.0321) + 16.308))
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
