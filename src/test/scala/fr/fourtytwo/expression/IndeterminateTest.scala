package fr.fourtytwo.expression

import fr.fourtytwo.exception.ParseException
import org.scalatest.funsuite.AnyFunSuite

class IndeterminateTest extends AnyFunSuite {

  test("Basic with Integers") {
    val indeterminate = Indeterminate("4*X^2")
    assert(indeterminate.toString.equals("(4.0 * X^2.0)"))
  }

  test("Basic with minus") {
    val indeterminate = Indeterminate("-4*X^2")
    assert(indeterminate.toString.equals("(-4.0 * X^2.0)"))
  }

  test("Basic with Double") {
    val indeterminate = Indeterminate("-4.00*X^2.00")
    assert(indeterminate.toString.equals("(-4.0 * X^2.0)"))
  }

  test("Basic with minus Double") {
    val indeterminate = Indeterminate("-4.123*X^2.87654")
    assert(indeterminate.toString.equals("(-4.123 * X^2.87654)"))
  }

  test("Zeros first") {
    val indeterminate = Indeterminate("000.123*X^000.87654")
    assert(indeterminate.toString.equals("(0.123 * X^0.87654)"))
  }

  test("Minus zeros first") {
    val indeterminate = Indeterminate("-0.123*X^0.87654")
    assert(indeterminate.toString.equals("(-0.123 * X^0.87654)"))
  }

  test("Only zeros") {
    val indeterminate = Indeterminate("-0.0*X^0.0")
    assert(indeterminate.toString.equals("(-0.0 * X^0.0)"))
  }

  test("Variable name length") {
    val indeterminate = Indeterminate("1.0*XYZ^1.0")

    assert(indeterminate.toString.equals("(1.0 * XYZ^1.0)"))
    assert(indeterminate.variable.toString.equals("XYZ"))
  }

  test("Minus degree should throws ParseException") {
    assertThrows[ParseException] {
      Indeterminate("1.0*XYZ^-1.0")
    }
  }

  test("Minus after constant") {
    assertThrows[ParseException] {
      Indeterminate("1.0-*XYZ^1.0")
    }
  }

  test("Minus after degree") {
    assertThrows[ParseException] {
      Indeterminate("1.0*XYZ^1.0-")
    }
  }

  test("Reverse with Integers") {
    val indeterminate = Indeterminate("X^2*4")
    assert(indeterminate.toString.equals("(4.0 * X^2.0)"))
  }

  test("Reverse with minus") {
    val indeterminate = Indeterminate("X^2*-4")
    assert(indeterminate.toString.equals("(-4.0 * X^2.0)"))
  }

  test("Reverse with minus double") {
    val indeterminate = Indeterminate("X^2*-4.001")
    assert(indeterminate.toString.equals("(-4.001 * X^2.0)"))
  }

  test("Start with letter") {
    assertThrows[ParseException] {
      Indeterminate("a1.0*XYZ^1.0-")
    }
  }

  test("Ends with letter") {
    assertThrows[ParseException] {
      Indeterminate("a1.0*XYZ^1.0asd")
    }
  }

  test("Bad constant") {
    assertThrows[ParseException] {
      Indeterminate("1.0.0*XYZ^1.0")
    }
  }

  test("Bad degree") {
    assertThrows[ParseException] {
      Indeterminate("1.0*XYZ^1.00.")
    }
  }
}
