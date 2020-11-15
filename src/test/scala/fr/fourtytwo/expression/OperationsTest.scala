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

  test("RealNumber * Indeterminate - 1") {
    val res = RealNumber(0.25) * Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(-12.0))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(1.0 * X^-12.0)"))
  }

  test("RealNumber / RealNumber") {
    val res = RealNumber(10.0) / RealNumber(4.0)
    assert(res.evaluate == 10.0 / 4.0)
  }

  test("RealNumber / Variable") {
    val res = RealNumber(10.0) / Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(10.0 * X^-1.0)"))
  }

  test("RealNumber / Indeterminate") {
    val res = RealNumber(10.0) / Indeterminate(RealNumber(2.0), Variable("X"), RealNumber(4.0))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(5.0 * X^-4.0)"))
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

  test("Variable / RealNumber") {
    val res = Variable("X") / RealNumber(4)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(0.25 * X^1.0)"))
  }

  test("Variable / Variable") {
    val res = Variable("X") / Variable("X")
    assert(res.isInstanceOf[RealNumber])
    assert(res.evaluate == 1.0)
  }

  test("Variable / OtherVariable") {
    assertThrows[EvaluateException] {
      Variable("X") / Variable("Y")
    }
  }

  test("Variable / Indeterminate - 1") {
    val res = Variable("X") / Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(0.112))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(0.25 * X^0.888)"), res.toString)
  }

  test("Variable / OtherIndeterminate") {
    assertThrows[EvaluateException] {
      Variable("X") / Indeterminate(RealNumber(4.0), Variable("Y"), RealNumber(13))
    }
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

  test("Indeterminate / RealNumber") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) / RealNumber(0.5)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(8.0 * X^13.0)"), res.toString)
  }

  test("Indeterminate / RealNumber = 0") {
    assertThrows[ArithmeticException] {
      Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) / RealNumber(0)
    }
  }

  test("Indeterminate / Variable") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) / Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(4.0 * X^12.0)"), res.toString)
  }

  test("Indeterminate / Variable = Zero") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1)) / Variable("X")
    assert(res.isInstanceOf[RealNumber])
    assert(res.evaluate == 4.0)
  }

  test("Indeterminate / Indeterminate - equal") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1.0)) /
      Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1.0))
    assert(res.isInstanceOf[RealNumber])
    assert(res.evaluate == 1.0)
  }

  test("Indeterminate / Indeterminate") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(2.0)) /
      Indeterminate(RealNumber(2.0), Variable("X"), RealNumber(1.0))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(2.0 * X^1.0)"), res.toString)
  }

}
