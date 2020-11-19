package fr.fourtytwo.token

import fr.fourtytwo.RPN
import org.scalatest.funsuite.AnyFunSuite
import fr.fourtytwo.Polynomial._

import scala.util.matching.Regex

class PolynomialOptimizeTest extends AnyFunSuite {

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

  test("Optimize - 1") {
    val expr = "8 * y^4 * (4 * y^2) / (2 * y^1) * (3 * y^3)"
    val opt = optimize(expr)
    assert(opt.equals("48.0 * y^8.0"), opt)
  }
}
