package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException
import org.scalatest.funsuite.AnyFunSuite

class OperationsTest extends AnyFunSuite {

  test("RealNumber * RealNumber") {
    val res = RealNumber(213.23123) * RealNumber(884.110030)
    assert(res.evaluate == 213.23123 * 884.110030)
  }

  test("RealNumber * RealNumber = 0") {
    val res = RealNumber(213.23123) * RealNumber(0)
    assert(res.evaluate == 0)
  }

  test("RealNumber * Variable") {
    val res = RealNumber(0.112) * Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(0.112 * X^1.0)"))
  }

  test("RealNumber * Indeterminate") {
    val res = RealNumber(0.5) * Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(132.22))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(2.0 * X^132.22)"))
  }

  test("Variable * RealNumber") {
    val res = Variable("X") * RealNumber(0.5)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(0.5 * X^1.0)"))
  }

  test("Variable * Variable") {
    val res = Variable("X") * Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(1.0 * X^2.0)"))
  }

  test("Variable * OtherVariable") {
    assertThrows[EvaluateException] {
      Variable("X") * Variable("Y")
    }
  }

  test("Variable * Indeterminate") {
    val res = Variable("X") * Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(4.0 * X^26.0)"), res.toString)
  }

  test("Indeterminate * RealNumber") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) * RealNumber(0.5)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(2.0 * X^13.0)"), res.toString)
  }

  test("Indeterminate * Variable") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(3.5)) * Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(4.0 * X^7.0)"), res.toString)
  }

  test("Indeterminate * OtherVariable") {
    assertThrows[EvaluateException] {
      Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(3.5)) * Variable("Y")
    }
  }

  test("Indeterminate * Indeterminate") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(3.5)) *
      Indeterminate(RealNumber(5), Variable("X"), RealNumber(3.5))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(20.0 * X^7.0)"), res.toString)
  }

  test("Indeterminate * OtherIndeterminate") {
    assertThrows[EvaluateException] {
      Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(3.5)) *
        Indeterminate(RealNumber(5), Variable("V"), RealNumber(3.5))
    }
  }

}
