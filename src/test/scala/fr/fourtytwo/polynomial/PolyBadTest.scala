package fr.fourtytwo.polynomial

import org.scalatest.funsuite.AnyFunSuite
import fr.fourtytwo.exception.{EvaluateException, ParseException}


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
      Polynomial("X ^ X + 4 + X = 3").evaluate
    }
  }


  test("Can't reduce - 1") {
    assertThrows[EvaluateException] {
      Polynomial("5 * X^1 + (4 * X^1 + 4) * 4 + 4 = 0")
    }
  }

  test("Can't reduce - 2") {
    assertThrows[EvaluateException] {
      Polynomial("(5 * X^1) + 4 / (4 * X^1 + 4) = 0")
    }
  }

  test("Can't reduce - 3") {
    assertThrows[EvaluateException] {
      Polynomial("(5 * X^1) + 4 ^ (4 * X^1 + 4) = 0")
    }
  }

  test("Negative degree - 1") {
    assertThrows[EvaluateException] {
      Polynomial("(5 * X^-1) + (4 * X^1 + 4) = 0").solve
    }
  }
  test("Negative degree - 2") {
    assertThrows[EvaluateException] {
      Polynomial("(5 * X^-1) + (4 * X^1 + 4) = 0").solve
    }
  }

  test("Negative degree - 3") {
    assertThrows[EvaluateException] {
      Polynomial("(5 / X^1) + 4 * 25 = 0").solve
    }
  }

  test("Big polynomial degree") {
    assertThrows[EvaluateException] {
      Polynomial("(5 * X^100) + 4 * 25 = 0").solve
    }
  }

  test("With separator") {
    assertThrows[EvaluateException] {
      Polynomial("(5 * X^100) + 4 * , 25 = 0").solve
    }
  }

  test("NoVars - 2") {
    assertThrows[EvaluateException] {
      Polynomial("4 * X^0 = 8 * X^0").solve
    }
  }

}
