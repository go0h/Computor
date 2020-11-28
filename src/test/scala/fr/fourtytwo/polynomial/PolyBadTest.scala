package fr.fourtytwo.polynomial

import fr.fourtytwo.exception.{EvaluateException, ParseException}
import org.scalatest.funsuite.AnyFunSuite

class PolyBadTest extends AnyFunSuite{

  test("No equal sign") {
    assertThrows[ParseException] {
      Polynomial("X^2 + 4")
    }
  }

  test("Two equal signs") {
    assertThrows[ParseException] {
      Polynomial("X^2 + 4 = 0 = 0")
    }
  }

  test("No variable") {
    assertThrows[EvaluateException] {
      Polynomial("2 + 2 * 2 = 6")
    }
  }

  test("Two variables") {
    assertThrows[EvaluateException] {
      Polynomial("X^2 + 4 + Y = 3")
    }
  }

  test("Variable ^ Variable - 1") {
    assertThrows[EvaluateException] {
      Polynomial("X^2^X + 4 + X = 3")
    }
  }

  test("Variable ^ Variable - 2") {
    assertThrows[EvaluateException] {
      Polynomial("X ^ X + 4 + X = 3")
    }
  }

  test("Variable % Variable - 2") {
    assertThrows[EvaluateException] {
      Polynomial("X % X + 4 + X = 3")
    }
  }

  test("Can't reduce - 2") {
    assertThrows[EvaluateException] {
      Polynomial("5 * X^1 + (4 * X^1 + 4) * 4 + 4 = 0")
    }
  }
}