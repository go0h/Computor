package fr.fourtytwo

import fr.fourtytwo.exception.ParseException
import fr.fourtytwo.token.Tokenizer
import org.scalatest.funsuite.AnyFunSuite

class RPNTest extends AnyFunSuite {

  val tokenizer = new Tokenizer

  def getResult(expr: String): Double = {
    val tokens = tokenizer.generateTokens(expr)
    RPN(tokens).solve
  }

  test("Empty expression") {
    assertThrows[ParseException] {
      getResult("")
    }
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

  test("Wiki hard - 1") {
    val expr = "3 + 4 * 2 / (1 - 5)^2"
    val res = getResult(expr)
    assert(res == 3.5, res.toString)
  }

  test("Wiki hard - 2") {
    val expr = "3.1 + 4 * 2 / (1 - 5)^2"
    val res = getResult(expr)
    assert(res == 3.6, res.toString)
  }

  test("EvalExpr- 1") {
    val expr = "50 - 45 * 14 % 2"
    val res = getResult(expr)
    assert(res == 50, res.toString)
  }

  test("EvalExpr - 1") {
    val expr = "50 - 45 * 14 % 2"
    val res = getResult(expr)
    assert(res == 50, res.toString)
  }

  test("EvalExpr - 2") {
    val expr = "20 - 99 / (96 - 30 * (57 - 76) - 43 % 20) - 17"
    val res = getResult(expr)
    assert(res == (20-99.0/(96-30*(57 - 76)-43%20)-17))
  }

  test("EvalExpr - 3 (ZERO)") {
    val expr = "0"
    val res = getResult(expr)
    assert(res == 0)
  }

  test("EvalExpr - 4") {
    val expr = "-93 + 3"
    val res = getResult(expr)
    assert(res == -93 + 3)
  }

  test("EvalExpr - 5") {
    val expr = "20+19*(13+59/(6+4)-97%11)-76"
    val res = getResult(expr)
    assert(res == (20+19*(13+59.0/(6+4)-97%11)-76))
  }

  test("EvalExpr - 6") {
    val expr = "-93 + -3"
    val res = getResult(expr)
    assert(res == -93 + -3)
  }

  test("EvalExpr - 7") {
    val expr = "96-(-30)*(57-76)-43%20"
    val res = getResult(expr)
    assert(res == 96-(-30)*(57-76)-43%20)
  }

  test("EvalExpr - 8") {
    val expr = "45-37"
    val res = getResult(expr)
    assert(res == 45-37)
  }

  test("EvalExpr - 9") {
    val expr = "86    -47   *(-26-75/(47-98/23)+17/(-         86  +          44*38))"
    val res = getResult(expr)
    assert(res == 86-47*(-26-75.0/(47-98.0/23)+17.0/(-86+44*38)))
  }

  test("EvalExpr - 10") {
    val expr = "86    -47   *(-26-75/(-47-98/23)+17/(-         86  +          44*-38))"
    val res = getResult(expr)
    assert(res == 86-47*(-26-75.0/(-47-98.0/23)+17.0/(-86+44*(-38))))
  }

  test("EvalExpr - 11") {
    val expr = "96- -30*(57-76)-43%20"
    val res = getResult(expr)
    assert(res == (96- -30*(57-76)-43%20))
  }

  test("Ultra unary minus - 1") {
    val expr = "96-(-(-(-30.3)))*(57-76)-43%20"
    val res = getResult(expr)
    assert(res == 96-(-(-(-30.3)))*(57-76)-43%20)
  }

  test("Ultra unary minus - 2") {
    val expr = "-96-(-(-(-30.3)))*(-57-76)-43%-20"
    val res = getResult(expr)
    assert(res == -96-(-(-(-30.3)))*(-57-76)-43%(-20))
  }

  test("Ultra unary minus - 3") {
    val expr = "-(-(-(-(-(-(-731.09))))))"
    val res = getResult(expr)
    assert(res == -(-(-(-(-(-(-731.09)))))))
  }



  /*
  FAIL
  96--30*(57-76)-43%20
  96---30*(57-76)-43%20
   */

}
