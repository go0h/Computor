package fr.fourtytwo.expression

import fr.fourtytwo.computor.Computor
import fr.fourtytwo.math._
import fr.fourtytwo.polynomial.SIMPLE_TOKENIZER
import org.scalatest.funsuite.AnyFunSuite

class ExpressionTest extends AnyFunSuite {

  def getExpr(expr: String): Expression = {
    val tokens = SIMPLE_TOKENIZER.generateTokens(expr)
    Computor(tokens).solve
  }

  test("Distinct values - 1") {
    val expr = "X * 3 + Y / 3 + a"
    val res = getExpr(expr)
    assert(res.countVars == 3)
  }

  test("Distinct values - 2") {
    val expr = "3 * 3 + 3.2 / 3 + 0.321"
    val res = getExpr(expr)
    assert(res.countVars == 0)
  }

  test("Distinct values - 3") {
    val expr = "3 * 3 + 3.2 / 3 + variable"
    val res = getExpr(expr)
    assert(res.countVars == 1)
  }

  test("Set values - 1") {
    val expr = "X * 3 + 6 / 3 + 1"
    val poly = getExpr(expr)
    val solvable = poly.setVar("X", RealNumber(3))
    assert(solvable.countVars == 0)
    assert(solvable.evaluate == 12.0, solvable.evaluate)
  }

  test("Set values - 2") {
    val expr = "X * X + A / B + C"
    val poly = getExpr(expr)
    assert(poly.countVars == 4)
    val solvable = poly
      .setVar("X", RealNumber(3))
      .setVar("A", RealNumber(6))
      .setVar("B", RealNumber(3))
      .setVar("C", RealNumber(1))
    assert(solvable.countVars == 0)
    assert(solvable.evaluate == 12.0, solvable.evaluate)
  }

  test("Set values - 3") {
    val expr = "3 * 3 + 6 / 3 + 1"
    val poly = getExpr(expr)
    val solvable = poly
      .setVar("X", RealNumber(3))
    assert(solvable.countVars == 0)
    assert(solvable.evaluate == 12.0, solvable.evaluate)
  }

}
