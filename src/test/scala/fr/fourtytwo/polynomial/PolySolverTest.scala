package fr.fourtytwo.polynomial

import scala.math.sqrt
import org.scalatest.funsuite.AnyFunSuite
import fr.fourtytwo.exception.EvaluateException

class PolySolverTest extends AnyFunSuite {

  def binomialSolve(a: Double, b: Double, c: Double): String = {

    val discriminant = (b * b) - 4 * a * c
    if (discriminant < 0)
      return ""

    val x1 = ((-b) + sqrt(discriminant)) / (2 * a)
    val x2 = ((-b) - sqrt(discriminant)) / (2 * a)

    s"""x1 = $x1
       |x2 = $x2""".stripMargin
  }

  def monomialSolve(a: Double, b: Double): String = ((-b) / a).toString

  def noVars(a: Double): String = {
    if (a != 0)
      throw new EvaluateException(s"The polynomial $toString is wrong")
    "All real numbers"
  }

  test("Subject test - 1") {
    val expr = "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0"
    val res = Polynomial(expr).solve
    assert(res.equals(binomialSolve(-9.3, 4, 4)))
  }

  test("Subject test - 2") {
    val expr = "5 * X^0 + 4 * X^1 = 4 * X^0"
    val res = Polynomial(expr).solve
    assert(res.equals(monomialSolve(4, 1)))
  }

  test("Subject test - 3") {
    assertThrows[EvaluateException] {
      Polynomial("8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0").solve
    }
  }

  test("Subject test - 4") {
    val expr = "5 + 4 * X + X^2 = X^2"
    val res = Polynomial(expr).solve
    assert(res.equals(monomialSolve(4, 5)))
  }

  test("NoVars wrong - 1") {
    assertThrows[EvaluateException] {
      Polynomial("5 + 4 * X + X^2 = X^2 + 4 * X").solve
    }
  }

  test("NoVars - 1") {
    val expr = "4 * X + X^2 = X^2 + 4 * X"
    val res = Polynomial(expr).solve
    assert(res.equals(noVars(0)))
  }

  test("Binomial test - 1") {
    val expr = "4 * X + X^2 = 0"
    val res = Polynomial(expr).solve
    assert(res.equals(binomialSolve(1, 4, 0)), binomialSolve(1, 4, 0))
  }

  test("Binomial test - 2") {
    val expr = "4 + 732 * X^2 = 0"
    val res = Polynomial(expr).solve
    assert(res.equals(binomialSolve(732, 0, 4)), binomialSolve(732, 0, 4))
  }

  test("Binomial test - 3") {
    val expr = "23 * X^2 = 0"
    val res = Polynomial(expr).solve
    assert(res.equals(binomialSolve(23, 0, 0)), binomialSolve(23, 0, 0))
  }

  test("Binomial test - 4") {
    val expr = "x^2 - 8*x + 12 = 0"
    val res = Polynomial(expr).solve
    val correct = binomialSolve(1, -8, 12)
    assert(res.equals(correct), correct)
  }

  //"5*x^2 + 3*x + 7 = 0"
  test("Negative discriminant - 1") {
    val expr = "5*x^2 + 3*x + 7 = 0"
    val res = Polynomial(expr).solve
    val correct = binomialSolve(5, 3, 7)
    assert(res.equals(correct), correct)
  }

  test("Binomial test - 5") {
    val expr =  "x^2 - 6*x + 9 = 0"
    val res = Polynomial(expr).solve
    val correct = binomialSolve(1, -6, 9)
    assert(res.equals(correct), correct)
  }

  test("Binomial test - 6") {
    val expr =  "15 - 2*x - x^2 = 0"
    val res = Polynomial(expr).solve
    val correct = binomialSolve(-1, -2, 15)
    assert(res.equals(correct), correct)
  }

  test("Binomial test - 7") {
    val expr =  "x^2 + 12*x + 36 = 0"
    val res = Polynomial(expr).solve
    val correct = binomialSolve(1, 12, 36)
    assert(res.equals(correct), correct)
  }

  test("Binomial test - 8") {
    val expr =  "x^2 + 9*x = 0"
    val res = Polynomial(expr).solve
    val correct = binomialSolve(1, 9, 0)
    assert(res.equals(correct), correct)
  }

  test("Binomial test - 9") {
    val expr = "x^2 - 7*x = 0"
    val res = Polynomial(expr).solve
    val correct = binomialSolve(1, -7, 0)
    assert(res.equals(correct), correct)
  }

  test("Binomial test - 10") {
    val expr = "5*x^2 + 30 = 0"
    val res = Polynomial(expr).solve
    val correct = binomialSolve(5, 0, 30)
    assert(res.equals(correct), correct)
  }

  test("Binomial test - 11") {
    val expr = "4*x^2 - 9 = 0"
    val res = Polynomial(expr).solve
    val correct = binomialSolve(4, 0, -9)
    assert(res.equals(correct), correct)
  }

}
