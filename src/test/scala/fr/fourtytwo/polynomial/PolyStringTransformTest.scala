package fr.fourtytwo.polynomial

import fr.fourtytwo.RPN
import fr.fourtytwo.expression.SIMPLE_TOKENIZER
import fr.fourtytwo.polynomial.Polynomial.normalizeRPNTokens
import fr.fourtytwo.token.TokenType.OPERATION
import fr.fourtytwo.token.Token
import org.scalatest.funsuite.AnyFunSuite

/** TEST transform random polynomial expression to special format
 * Example: {{{3x^2 + x + 3 = 0 => (3.0 * x^2.0) + (1.0 * X^1) + 3 = 0}}}
 */
class PolyStringTransformTest extends AnyFunSuite {

  def normalizePolynomial(expression: String): String = {

    val tokens: Array[Token] = SIMPLE_TOKENIZER.generateTokens(expression)

    val leftRPN: Array[Token] =
      RPN.convertToRPN(tokens.slice(0, tokens.indexOf(Token("=", OPERATION))))
    val rightRPN: Array[Token] =
      RPN.convertToRPN(tokens.slice(tokens.indexOf(Token("=", OPERATION)) + 1, tokens.length))

    val leftExpr: String = normalizeRPNTokens(leftRPN)
    val rightExpr: String = normalizeRPNTokens(rightRPN)

    s"$leftExpr = $rightExpr"
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
