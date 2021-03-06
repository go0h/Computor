package fr.fourtytwo.token

import fr.fourtytwo.exception.ParseException
import org.scalatest.funsuite.AnyFunSuite

class TokenizerTest extends AnyFunSuite {

  val tokenizer: Tokenizer = Tokenizer()

  test("Basic brackets - 1") {
    val expr = "((5)+4*X+X^2)=X^2"
    val tokens = tokenizer.generateTokens(expr)
    val str = tokens.fold("")((x, y) => x.toString + y.toString).toString
    assert(expr.equals(str))
  }

  test("Basic brackets error - 1") {
    assertThrows[ParseException] {
      tokenizer.generateTokens("((5))+4 * X + (X^2= X^2")
    }
  }

  test("Basic brackets error - 2") {
    assertThrows[ParseException] {
      tokenizer.generateTokens("((5))+4*X+(X^2))= X^2")
    }
  }

  test("Unknown tokens") {
    assertThrows[ParseException] {
      tokenizer.generateTokens("&+4 * X + X^2= X^2")
    }
    assertThrows[ParseException] {
      tokenizer.generateTokens("4 * С + X^2= X^2")
    }
  }

}
