package fr.fourtytwo.polynomial

import org.scalatest.funsuite.AnyFunSuite
import fr.fourtytwo.polynomial.PolynomialReducer.toOptimalExpression

/** TEST transform random polynomial expression to special format
 * Example: {{{3x^2 + x + 3 = 0 => (3 * x^2) + (1 * X^1) + 3 = 0}}}
 */
class PolyStringTransformTest extends AnyFunSuite {

  def normalizePolynomial(expr: String): String = {

    val leftExpr = toOptimalExpression(expr.substring(0, expr.indexOf("="))).evaluate
    val rightExpr = toOptimalExpression(expr.substring(expr.indexOf("=") + 1)).evaluate

    s"$leftExpr = $rightExpr"
  }

  test("Basic convention - 1") {
    val expr = "(4.0 * X^2.0) - (-4.0 * X^1.0) + 4.0 = 0.0"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("4 * X^2 + 4 * X + 4 = 0"), norm)
  }

  test("Basic convention - 2") {
    val expr = "(X^2.0 * 4) + (4 * X^2) - (X^2 * 4) + (3 * X^3) + 3 = 3"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("4 * X^2 + 3 * X^3 + 3 = 3"), norm)
  }

  test("Basic convention - 3") {
    val expr = "4 * X^2 + 3 * X + 5 = 123.43"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("4 * X^2 + 3 * X + 5 = 123.43"), norm)
  }

  test("Basic convention - 4") {
    val expr = "X^2 + X - 32.5830001 = X"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("X^2 + X + -32.5830001 = X"), norm)
  }

  test("Basic convention - 5") {
    val expr = "(4 * X^2.0) - (-4 * X^1) + 4 = 1"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("4 * X^2 + 4 * X + 4 = 1"), norm)
  }

  test("Basic convention - 6") {
    val expr = "X^7 - X^6 - X^5 - X^4 - X^3 - X^2 - X + 4 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("X^7 + -1 * X^6 + -1 * X^5 + -1 * X^4 + -1 * X^3 + -1 * X^2 + -X + 4 = 77"), norm)
  }

  test("Basic convention - 7") {
    val expr = "X ^ 4 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("X^4 = 77"), norm)
  }

  test("Basic convention - 8") {
    val expr = "X * 4 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("4 * X = 77"), norm)
  }

  test("Division convention - 1") {
    val expr = "4.0 / X^(-2.0) = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("4 * X^2 = 77"), norm)
  }

  test("Division convention - 2") {
    val expr = "X^(-2.0) / 2 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("0.5 * X^-2 = 77"), norm)
  }

  test("Division convention - 3") {
    val expr = "X^14.0 / 4 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("0.25 * X^14 = 77"), norm)
  }

  test("Division convention - 4") {
    val expr = "X / 4 = 77"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("0.25 * X = 77"), norm)
  }

  test("Subject test - 1") {
    val expr = "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("5 + 4 * X + -9.3 * X^2 = 1"), norm)
  }

  test("Subject test - 2") {
    val expr = "5 * X^0 + 4 * X^1 = 4 * X^0"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("5 + 4 * X = 4"), norm)
  }

  test("Subject test - 3") {
    val expr = "8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("8 + -6 * X + -5.6 * X^3 = 3"), norm)
  }

  test("Subject test - 4") {
    val expr = "5 + 4 * X + X^2= X^2"
    val norm = normalizePolynomial(expr)
    assert(norm.equals("5 + 4 * X + X^2 = X^2"), norm)
  }

}
