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

}
