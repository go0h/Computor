package fr.fourtytwo.expression

import org.scalatest.funsuite.AnyFunSuite

class RealNumberTest extends AnyFunSuite {

  test("RealNumber + RealNumber") {
    val res = RealNumber(3.14) + RealNumber(0.86)
    assert(res == 3.14 + 0.86)
  }

  test("RealNumber + Variable") {
    val res = RealNumber(3.14) + Variable("Y")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3.14 + Y"), res)
  }

  test("RealNumber = 0 + Variable") {
    val res = RealNumber(0) + Variable("Y")
    assert(res.isInstanceOf[Variable])
    assert(res.toString.equals("Y"), res)
  }

  test("RealNumber + Indeterminate") {
    val res = RealNumber(3.14) + Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1.0))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3.14 + 4.0 * X"), res)
  }

  test("RealNumber = 0 + Indeterminate") {
    val res = RealNumber(0) + Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1.0))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4.0 * X"), res)
  }

  test("RealNumber + ComplexNumber") {
    val res = RealNumber(4) + ComplexNumber(8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("12.0 + 2.0i"), res)
  }

  test("RealNumber = 0 + ComplexNumber") {
    val res = RealNumber(0) + ComplexNumber(8, -2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("8.0 - 2.0i"), res)
  }

  test("RealNumber - RealNumber") {
    val res = RealNumber(3.14) - RealNumber(0.86)
    assert(res == 3.14 - 0.86)
  }

  test("RealNumber - Variable") {
    val res = RealNumber(3.14) - Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3.14 - X"), res)
  }

  test("RealNumber - -Variable") {
    val res = RealNumber(3.14) - Variable("-X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3.14 - -X"), res)
  }

  test("RealNumber - Indeterminate") {
    val res = RealNumber(3.14) - Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1.0))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3.14 - 4.0 * X"), res)
  }

  test("RealNumber - ComplexNumber") {
    val res = RealNumber(4) - ComplexNumber(8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("-4.0 - 2.0i"), res)
  }

  test("RealNumber = 0 - ComplexNumber") {
    val res = RealNumber(0) - ComplexNumber(8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("-8.0 - 2.0i"), res)
  }

  test("RealNumber * RealNumber") {
    val res = RealNumber(213.23123) * RealNumber(884.110030)
    assert(res == 213.23123 * 884.110030)
  }

  test("RealNumber * RealNumber = 0") {
    val res = RealNumber(213.23123) * RealNumber(0)
    assert(res == 0)
  }

  test("RealNumber * Variable") {
    val res = RealNumber(0.112) * Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("0.112 * X"))
  }

  test("RealNumber * -Variable") {
    val res = RealNumber(3.14) * Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-3.14 * X"), res)
  }

  test("RealNumber * Indeterminate") {
    val res = RealNumber(0.5) * Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(132.22))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("2.0 * X^132.22"))
  }

  test("RealNumber * Indeterminate - 1") {
    val res = RealNumber(0.25) * Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(-12.0))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("X^-12.0"))
  }

  test("RealNumber * ComplexNumber") {
    val res = RealNumber(4) * ComplexNumber(8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("32.0 + 8.0i"), res)
  }

  test("RealNumber * -ComplexNumber") {
    val res = RealNumber(4) * ComplexNumber(-8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("-32.0 + 8.0i"), res)
  }

  test("RealNumber / RealNumber") {
    val res = RealNumber(10.0) / RealNumber(4.0)
    assert(res == 10.0 / 4.0)
  }

  test("RealNumber / Variable") {
    val res = RealNumber(10.0) / Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("10.0 * X^-1.0"))
  }

  test("RealNumber / -Variable") {
    val res = RealNumber(10.0) / Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-10.0 * X^-1.0"))
  }

  test("RealNumber / Indeterminate") {
    val res = RealNumber(10.0) / Indeterminate(RealNumber(2.0), Variable("X"), RealNumber(4.0))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("5.0 * X^-4.0"))
  }

  test("RealNumber / ComplexNumber") {
    val res = RealNumber(4) / ComplexNumber(2, 4)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("0.4 - 0.8i"), res)
  }

}
