package fr.fourtytwo.parser

import fr.fourtytwo.exception.ParseException
import fr.fourtytwo.expression.Indeterminate
import org.scalatest.funsuite.AnyFunSuite

class ParserTest extends AnyFunSuite {

  test("Remove spaces") {
    val expr = new Parser("5 + 4 * X + X^2= X^2").expression
    assert(!expr.contains(" "))
  }

  test("Remove tabs") {
    val expr = new Parser("5  + 4 * X + X^2= X^2").expression
    assert(!expr.contains("\t"))
  }

  test("Remove tabs and spaces") {
    val expr = new Parser("5  + 4 * X + X^2= X^2").expression
    assert(!expr.contains("\t ".toCharArray))
  }

  test("Basic brackets") {
      val expr = new Parser("((5)+4 * X + X^2)= X^2").expression
      assert(!expr.contains(" "))
  }

  test("Basic brackets error - 1") {
    assertThrows[ParseException] {
      new Parser("((5))+4 * X + (X^2= X^2")
    }
  }

  test("Basic brackets error - 2") {
    assertThrows[ParseException] {
      new Parser("((5))+4*X+(X^2))= X^2")
    }
  }

}
