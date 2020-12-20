package fr.fourtytwo.polynomial

import org.scalatest.funsuite.AnyFunSuite
import fr.fourtytwo.polynomial.PolynomialReducer._


class PolyOptimizeTest extends AnyFunSuite {

  def optimize(expression: String): String = {
    toOptimalExpression(expression)
      .toString
      .replaceAll("[()]", "")
  }

  test("Basic division - 1") {
    val expr = "(4 * y^2) + (4 * y^2) / (-4 * y^2) + (3 * y^3) + 3"
    val opt = optimize(expr)
    assert(opt.equals("4.0 * y^2.0 + -1.0 + 3.0 * y^3.0 + 3.0"), opt)
  }

  test("Basic division - 2") {
    val expr = "(8 * y^4) / (4 * y^2) / (2 * y^1) * (3 * y^3) + 3"
    val opt = optimize(expr)
    assert(opt.equals("3.0 * y^4.0 + 3.0"), opt)
  }

  test("Basic - 1") {
    val expr = "8 * y^4 * (4 * y^2) / (2 * y^1) * (3 * y^3)"
    val opt = optimize(expr)
    assert(opt.equals("48.0 * y^8.0"), opt)
  }

  test("Basic - 2") {
    val expr = "(8 * y^4) * (4 * y^2) / 2 * y + 3 * y^3"
    val opt = optimize(expr)
    assert(opt.equals("16.0 * y^7.0 + 3.0 * y^3.0"), opt)
  }

  test("Basic - 3") {
    val expr = "(8 * y^0.12) * (-4 * y^-0.302) / (2 * y^1)"
    val opt = optimize(expr)
    assert(opt.equals("-16.0 * y^-1.182"), opt)
  }

  test("Zero on right side") {
    val expr = "y^4 * (4 * y^4) / y^2 = 0"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(4.0 * y^6.0) = 0.0"), opt)
  }

  test("Non Zero on right side") {
    val expr = "8 * y^4 * (4 * y^2) / (2 * y^1) * (3 * y^3) = 4"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(48.0 * y^8.0) + -4.0 = 0.0"), opt)
  }


  test("Optimize - 1") {
    val expr = "4 + 8 * X^4 / (4 * X^2) * (2 * X^1) + X^3.23 = 4 - 10 + X^12"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(-1.0 * X^12.0) + (1.0 * X^3.23) + (4.0 * X^3.0) + 10.0 = 0.0"), opt)
  }

  test("Optimize - 2") {
    val expr = "4 - 8 * X^4 / (4 * X^2) * (2 * X^1) + X^3.23 = 4 - 10 + X^12"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(-1.0 * X^12.0) + (1.0 * X^3.23) + (-4.0 * X^3.0) + 10.0 = 0.0"), opt)
  }

  test("Unary Indeterminate - 1") {
    val expr = "4 - -8 * X^4 / (4 * X^2) * (2 * X^1) + X^3.23 = 4 - 10 + X^12"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(-1.0 * X^12.0) + (1.0 * X^3.23) + (4.0 * X^3.0) + 10.0 = 0.0"), opt)
  }

  test("Unary Indeterminate - 2") {
    val expr = "4 - -8 * X^4 / (4 * X^2) * (-2 * X^1) + X^3.23 = 4 - 10 + X^12"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(-1.0 * X^12.0) + (1.0 * X^3.23) + (-4.0 * X^3.0) + 10.0 = 0.0"), opt)
  }

  test("Unary Realnumber - 1") {
    val expr = "4 - -8 * X^4 / (4 * X^2) * (-2 * X^1) + X^3.23 = -4 - 10 + X^12"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(-1.0 * X^12.0) + (1.0 * X^3.23) + (-4.0 * X^3.0) + 18.0 = 0.0"), opt)
  }

  test("Optimize - 3") {
    val expr = "X^2 - X^2 + X + 13 * 2 = 0"
    val opt = Polynomial(expr).toString
    assert(opt.equals("X + 26.0 = 0.0"), opt)
  }

  test("Only zero on the left side") {
    val expr = "0 = X^2 - X^2 + X + 13 * 2"
    val opt = Polynomial(expr).toString
    assert(opt.equals("-X + -26.0 = 0.0"), opt)
  }

  test("Optimize - 4") {
    val expr = "X^2 + X + 13 * 2 = X^2"
    val opt = Polynomial(expr).toString
    assert(opt.equals("X + 26.0 = 0.0"), opt)
  }

  test("Optimize - 5") {
    val expr = "X^2 * 4 + X + 13 * 2 = X^2"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(3.0 * X^2.0) + X + 26.0 = 0.0"), opt)
  }

  test("Optimize - 6") {
    val expr = "X^2 / 4 + X + 13 * 2 = X^2"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(-0.75 * X^2.0) + X + 26.0 = 0.0"), opt)
  }

  test("Optimize - 7") {
    val expr = "X^2^4 + X + 13 * 2 = 4^2"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(1.0 * X^16.0) + X + 10.0 = 0.0"), opt)
  }

  test("Optimize - 8") {
    val expr = "(4 * X^2)^2 + X + 13 * 2 = 4^2"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(16.0 * X^4.0) + X + 10.0 = 0.0"), opt)
  }

  test("Subject test - 1") {
    val expr = "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0"
    val norm = Polynomial(expr).toString
    assert(norm.equals("(-9.3 * X^2.0) + (4.0 * X^1.0) + 4.0 = 0.0"), norm)
  }

  test("Subject test - 2") {
    val expr = "5 * X^0 + 4 * X^1 = 4 * X^0"
    val norm = Polynomial(expr).toString
    assert(norm.equals("(4.0 * X^1.0) + 1.0 = 0.0"), norm)
  }

  test("Subject test - 3") {
    val expr = "8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0"
    val norm = Polynomial(expr).toString
    assert(norm.equals("(-5.6 * X^3.0) + (-6.0 * X^1.0) + 5.0 = 0.0"), norm)
  }

  test("Subject test - 4") {
    val expr = "5 + 4 * X + X^2= X^2"
    val norm = Polynomial(expr).toString
    assert(norm.equals("(4.0 * X^1.0) + 5.0 = 0.0"), norm)
  }

  test("Negative braces - 1") {
    val expr =   "5 * X^1 - ((4 * X^1 + 4) + 4) = 0"
    val norm = Polynomial(expr).toString
    assert(norm.equals("(1.0 * X^1.0) + -8.0 = 0.0"), norm)
  }

  test("Negative braces - 2") {
    val expr =   "5 * X^1 - (4 * X^1 + 4) + 4 = 0"
    val norm = Polynomial(expr).toString
    assert(norm.equals("(1.0 * X^1.0) = 0.0"), norm)
  }

  test("Negative braces - 3") {
    val expr =   "-(5 * X^1) - (4 * X^1 + 4) + 4 = 0"
    val norm = Polynomial(expr).toString
    assert(norm.equals("(-9.0 * X^1.0) = 0.0"), norm)
  }

  test("Negative braces - 4") {
    val expr =   "-((5 * X^1) - ((4 * X^1 + 4) + 4)) = 0"
    val norm = Polynomial(expr).toString
    assert(norm.equals("(-1.0 * X^1.0) + 8.0 = 0.0"), norm)
  }

  test("Negative variable") {
    val expr = "8 * y^0.12 * -4 * y^1 + y^1 + 4 * -y^3 + 31 = 0"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(-4.0 * y^3.0) + (-32.0 * y^1.12) + (1.0 * y^1.0) + 31.0 = 0.0"), opt)
  }



}
