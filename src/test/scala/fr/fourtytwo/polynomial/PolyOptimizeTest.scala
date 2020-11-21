package fr.fourtytwo.polynomial

import fr.fourtytwo.polynomial.Polynomial._
import org.scalatest.funsuite.AnyFunSuite

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
    assert(opt.equals("(4.0 * y^6.0) = 0"), opt)
  }

  test("Non Zero on right side") {
    val expr = "8 * y^4 * (4 * y^2) / (2 * y^1) * (3 * y^3) = 4"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(48.0 * y^8.0) + -4.0 = 0"), opt)
  }


  test("Optimize - 1") {
    val expr = "4 + 8 * X^4 / (4 * X^2) * (2 * X^1) + X^3.23 = 4 - 10 + X^12"
    val opt = Polynomial(expr).toString
    assert(opt.equals("4.0 + (4.0 * X^3.0) + (1.0 * X^3.23) + 6.0 + (-1.0 * X^12.0) = 0"), opt)
  }

  test("Optimize - 2") {
    val expr = "4 - 8 * X^4 / (4 * X^2) * (2 * X^1) + X^3.23 = 4 - 10 + X^12"
    val opt = Polynomial(expr).toString
    assert(opt.equals("4.0 + (-4.0 * X^3.0) + (1.0 * X^3.23) + 6.0 + (-1.0 * X^12.0) = 0"), opt)
  }

  test("Unary Indeterminate - 1") {
    val expr = "4 - -8 * X^4 / (4 * X^2) * (2 * X^1) + X^3.23 = 4 - 10 + X^12"
    val opt = Polynomial(expr).toString
    assert(opt.equals("4.0 + (4.0 * X^3.0) + (1.0 * X^3.23) + 6.0 + (-1.0 * X^12.0) = 0"), opt)
  }

  test("Unary Indeterminate - 2") {
    val expr = "4 - -8 * X^4 / (4 * X^2) * (-2 * X^1) + X^3.23 = 4 - 10 + X^12"
    val opt = Polynomial(expr).toString
    assert(opt.equals("4.0 + (-4.0 * X^3.0) + (1.0 * X^3.23) + 6.0 + (-1.0 * X^12.0) = 0"), opt)
  }

  test("Unary Realnumber - 1") {
    val expr = "4 - -8 * X^4 / (4 * X^2) * (-2 * X^1) + X^3.23 = -4 - 10 + X^12"
    val opt = Polynomial(expr).toString
    assert(opt.equals("4.0 + (-4.0 * X^3.0) + (1.0 * X^3.23) + 14.0 + (-1.0 * X^12.0) = 0"), opt)
  }

  test("Optimize - 3") {
    val expr = "X^2 - X^2 + X + 13 * 2 = 0"
    val opt = Polynomial(expr).toString
    assert(opt.equals("(1.0 * X^1.0) + 26.0 = 0"), opt)
  }

}
