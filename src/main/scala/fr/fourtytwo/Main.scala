package fr.fourtytwo

import fr.fourtytwo.exception._
import fr.fourtytwo.token._
import fr.fourtytwo.expression._

import scala.collection.mutable.ArrayBuffer

object Main {

  def test(tokens: Array[String]): Unit = {

    var tk = tokens.toList
    val expr: ArrayBuffer[Expression] = ArrayBuffer[Expression]()

    while (tk.nonEmpty) {

      tk match {
        case List(c, "*", v, "^", d, _*) => {
          expr.append(new Indeterminate(c.toDouble, "*", v, d.toDouble))
          print(new Indeterminate(c.toDouble, "*", v, d.toDouble).toString)
          tk = tk.takeRight(tk.length - 5)
        }
        case List("+", _*) | List("-", _*) => {
          expr.append(Operator(null, tk.head, null))
          tk = tk.tail
        }
        case List(Number(x), _*) => {
          expr.append(Number(x))
          tk = tk.tail
        }
        case List(RealNumber(x), _*) => {
          expr.append(RealNumber(x))
          tk = tk.tail
        }
        case List("=", _*) => {
          println(tk.head)
          tk = tk.tail
        }
        case _ => {
          println("NOT OK = " + tk.head)
          tk = tk.tail
        }
      }
    }
    println("\n" + expr.mkString(" "))
  }

  def main(args: Array[String]): Unit = {

    TokenType.values.toArray.map(x => (x, x.toString.r)).toMap

    val expr = "5.0007*X^2+4*X^1-9.3*X^2"
    val tokenizer = Tokenizer()

    val expr1 = "3 + 4 * 2 / (1 - 5)^2"

    try {
      val tokens = tokenizer.generateTokens(expr1)
      val rpn = RPN(tokens)
      println(rpn.getTokens.map(x => x.expr).mkString(" ") + " = " + rpn.solve)

    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex1: Exception => println("Exception " + ex1.getMessage)
    }
  }
}
