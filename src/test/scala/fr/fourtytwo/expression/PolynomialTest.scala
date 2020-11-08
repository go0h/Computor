package fr.fourtytwo.expression

import fr.fourtytwo.RPN
import fr.fourtytwo.token._
import org.scalatest.funsuite.AnyFunSuite
import fr.fourtytwo.expression.Indeterminate._

class PolynomialTest extends AnyFunSuite {

  val tokenizer: Tokenizer = Tokenizer()

  def normalizePolynomial(expression: String): String = {
    val poly = Polynomial(expression)
    poly.normExp
  }

  test("Basic convention - 1") {
    val expr = "(4 * X^2.0) - (-4 * X^1) + 4 = 0"
    val norm = normalizePolynomial(expr)
    assert(expr.equals(norm))
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
}
