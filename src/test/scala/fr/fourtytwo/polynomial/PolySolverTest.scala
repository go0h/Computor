package fr.fourtytwo.polynomial

import scala.math.sqrt
import org.scalatest.funsuite.AnyFunSuite
import fr.fourtytwo.exception.EvaluateException
import fr.fourtytwo.expression.{ComplexNumber, Operable, RealNumber}

class PolySolverTest extends AnyFunSuite {

  def binomialSolve(a: Double, b: Double, c: Double): String = {

    val discriminant = (b * b) - 4 * a * c
    if (discriminant < 0) {
      val compDisc = ComplexNumber(0, sqrt(-discriminant))
      val x1 = (RealNumber(-b) + compDisc).asInstanceOf[Operable] / RealNumber(2 * a)
      val x2 = (RealNumber(-b) - compDisc).asInstanceOf[Operable] / RealNumber(2 * a)

      s"""x1 = $x1
         |x2 = $x2""".stripMargin
    }
    else if (discriminant == 0) {
      val res = (-b) / (2 * a)
      s"x = ${if (res == 0) 0.0 else res}"
    }
    else {
      val x1 = ((-b) + sqrt(discriminant)) / (2 * a)
      val x2 = ((-b) - sqrt(discriminant)) / (2 * a)

      s"""x1 = $x1
         |x2 = $x2""".stripMargin
    }
  }

  def monomialSolve(a: Double, b: Double): String = {
    val res = (-b) / a
    s"x = ${if (res == 0) 0.0 else res}"
  }

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
    assert(res.equals(monomialSolve(4, 1)), monomialSolve(4,1))
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

  test("Binomial test - 13") {
    val expr = "X % X + 4 + X = 3"
    val res = Polynomial(expr).solve
    assert(res.equals(monomialSolve(1, 1)), monomialSolve(1, 1))
  }

}
