package fr.fourtytwo.expression

import fr.fourtytwo.RPN
import fr.fourtytwo.exception._
import fr.fourtytwo.token.{TokenType, _}
import org.scalatest.funsuite.AnyFunSuite

import scala.util.matching.Regex

class PolynomialTest extends AnyFunSuite {

  val matchers: Map[TokenType.Value, Regex] = TokenType.getSimpleTypesWithRegex
  val tokenizer: Tokenizer = Tokenizer(matchers)

  def normalizePolynomial(expression: String): String = {
    val poly = Polynomial(expression)
    poly.normExp
  }

  def getResult(expr: String): Double = {
    val tokens = tokenizer.generateTokens(expr)
    RPN(tokens).solve.evaluate
  }

  def getResultAlter(expr: String): Double = {
    val rpnTokens = RPN(tokenizer.generateTokens(expr)).tokens
    val normalExpression = Polynomial.normalize(rpnTokens)
    getResult(normalExpression)
  }


  test("Basic convention - 1") {
    val expr = "(4 * X^2.0) - (-4 * X^1) + 4 = 0"
    val norm = normalizePolynomial(expr)
    assert(expr.equals(norm), norm)
  }

  test("Basic convention - 2") {
    val expr = "(X^2.0 * 4) + (4 * X^2) - (X^2 * 4) + (3 * X^3) + 3 = 3"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("(4 * X^2.0) + (4 * X^2) - (4 * X^2) + (3 * X^3) + 3 = 3"), norm)
  }

  test("Basic convention - 3") {
    val expr = "4 * X^2 + 3 * X + 5 = 123.43"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("(4 * X^2) + (3 * X^1) + 5 = 123.43"), norm)
  }

  test("Basic convention - 4") {
    val expr = "X^2 + X - 32.5830001 = X"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("(1 * X^2) + (1 * X^1) - 32.5830001 = (1 * X^1)"), norm)
  }

  test("Basic convention - 5") {
    val expr = "(4 * X^2.0) - (-4 * X^1) + 4 = 1"
    val norm = normalizePolynomial(expr)
    assert(norm.equals(expr), norm)
  }

  test("Basic convention - 6") {
    val expr = "X^7 - X^6 - X^5 - X^4 - X^3 - X^2 - X + 4 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("(1 * X^7) - (1 * X^6) - (1 * X^5) - (1 * X^4) - (1 * X^3) - (1 * X^2) - (1 * X^1) + 4 = 77"), norm)
  }

  test("Basic convention - 7") {
    val expr = "X ^ 4 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("(1 * X^4) = 77"), norm)
  }

  test("Basic convention - 8") {
    val expr = "X * 4 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("(4 * X^1) = 77"), norm)
  }

  test("No equal sing") {
    assertThrows[ParseException] {
      normalizePolynomial("X^2 + 4")
    }
  }

  test("Two equal sings") {
    assertThrows[ParseException] {
      normalizePolynomial("X^2 + 4 = 0 = 0")
    }
  }

  test("No variable") {
    assertThrows[EvaluateException] {
      normalizePolynomial("2 + 2 * 2 = 6")
    }
  }

  test("Two variables") {
    assertThrows[EvaluateException] {
      normalizePolynomial("X^2 + 4 + Y = 3")
    }
  }

  test("Division convention - 1") {
    val expr = "4.0 / X^(-2.0) = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("(4.0 * X^2.0) = 77"), norm)
  }

  test("Division convention - 2") {
    val expr = "X^(-2.0) / 2 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("(0.5 * X^-2.0) = 77"), norm)
  }

  test("Division convention - 3") {
    val expr = "X^14.0 / 4 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("(0.25 * X^14.0) = 77"), norm)
  }

  test("Division convention - 4") {
    val expr = "X / 4 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("(0.25 * X^1) = 77"), norm)
  }

}
