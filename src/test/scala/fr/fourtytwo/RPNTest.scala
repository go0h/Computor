package fr.fourtytwo

import fr.fourtytwo.expression.Indeterminate
import fr.fourtytwo.token.Tokenizer
import org.scalatest.funsuite.AnyFunSuite

class RPNTest extends AnyFunSuite {

  val tokenizer = new Tokenizer

  def getResult(expr: String): Double = {
    val tokens = tokenizer.generateTokens(expr)
    RPN(tokens).solve
  }

  test("Wiki example") {
    val expr = "(1 + 2) * 4 + 3"
    val res = getResult(expr)
    assert(res == 15, res.toString)
  }

  test("Classic") {
    val expr = "2 + 2 * 2"
    val res = getResult(expr)
    assert(res == 6, res.toString)
  }

  test("Wiki hard") {
    val expr = "3 + 4 * 2 / (1 - 5)^2"
    val res = getResult(expr)
    assert(res == 3.5, res.toString)
  }

}
