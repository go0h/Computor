package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException
import org.scalatest.funsuite.AnyFunSuite

class OperationsTest extends AnyFunSuite {

  ////////////////////////////////////////
  ////////////// REAL NUMBER /////////////
  ////////////////////////////////////////
  test("RealNumber + RealNumber") {
    val res = RealNumber(3.14) + RealNumber(0.86)
    assert(res.evaluate == 3.14 + 0.86)
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
    assert(res.toString.equals("3.14 + (4.0 * X^1.0)"), res)
  }

  test("RealNumber = 0 + Indeterminate") {
    val res = RealNumber(0) + Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1.0))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(4.0 * X^1.0)"), res)
  }

  test("RealNumber - RealNumber") {
    val res = RealNumber(3.14) - RealNumber(0.86)
    assert(res.evaluate == 3.14 - 0.86)
  }

  test("RealNumber - Variable") {
    val res = RealNumber(3.14) - Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3.14 - X"), res)
  }

  test("RealNumber - Indeterminate") {
    val res = RealNumber(3.14) - Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1.0))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3.14 - (4.0 * X^1.0)"), res)
  }

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

  ////////////////////////////////////////
  //////////////// VARIABLE //////////////
  ////////////////////////////////////////
  test("Variable + RealNumber") {
    val res = Variable("X") + RealNumber(0.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X + 0.5"))
  }

  test("Variable + RealNumber = 0") {
    val res = Variable("X") + RealNumber(0)
    assert(res.isInstanceOf[Variable])
    assert(res.toString.equals("X"))
  }

  test("Variable + Variable") {
    val res = Variable("X") + Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(2.0 * X^1.0)"), res)
  }

  test("Variable + OtherVariable") {
    assertThrows[EvaluateException] {
      Variable("X") + Variable("Y")
    }
  }

  test("Variable + Indeterminate") {
    val res = Variable("X") + Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X + (4.0 * X^13.0)"), res)
  }

  test("Variable + Indeterminate. Degree == 1") {
    val res = Variable("X") + Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(5.0 * X^1.0)"), res)
  }

  test("Variable + OtherIndeterminate") {
    assertThrows[EvaluateException] {
      Variable("X") + Indeterminate(RealNumber(4.0), Variable("Y"), RealNumber(1))
    }
  }

  test("Variable - RealNumber") {
    val res = Variable("X") - RealNumber(0.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X - 0.5"))
  }

  test("Variable - RealNumber = 0") {
    val res = Variable("X") - RealNumber(0)
    assert(res.isInstanceOf[Variable])
    assert(res.toString.equals("X"))
  }

  test("Variable - Variable") {
    val res = Variable("X") - Variable("X")
    assert(res.isInstanceOf[RealNumber])
    assert(res.evaluate == 0)
  }

  test("Variable - OtherVariable") {
    assertThrows[EvaluateException] {
      Variable("X") - Variable("Y")
    }
  }

  test("Variable - OtherIndeterminate") {
    assertThrows[EvaluateException] {
      Variable("X") - Indeterminate(RealNumber(4.0), Variable("Y"), RealNumber(13))
    }
  }

  test("Variable - Indeterminate") {
    val res = Variable("X") - Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X - (4.0 * X^13.0)"))
  }

  test("Variable - Indeterminate^1") {
    val res = Variable("X") - Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(-3.0 * X^1.0)"), res)
  }

  test("Variable - -Indeterminate^1") {
    val res = Variable("X") - Indeterminate(RealNumber(-4.0), Variable("X"), RealNumber(1))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(5.0 * X^1.0)"), res)
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
    val res = Variable("X") * Indeterminate(RealNumber(4), Variable("X"), RealNumber(13))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(4.0 * X^14.0)"), res.toString)
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

  ////////////////////////////////////////
  /////////// INDETERMINATE //////////////
  ////////////////////////////////////////
  test("Indeterminate + RealNumber") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) + RealNumber(0.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("(4.0 * X^13.0) + 0.5"), res.toString)
  }

  test("Indeterminate + RealNumber = 0") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) + RealNumber(0.0)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(4.0 * X^13.0)"), res.toString)
  }

  test("Indeterminate + Variable") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) + Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("(4.0 * X^13.0) + X"), res)
  }

  test("Indeterminate + OtherVariable") {
    assertThrows[EvaluateException] {
      Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) + Variable("Y")
    }
  }

  test("Indeterminate + Variable - Degree == 1") {
    val res = Indeterminate(RealNumber(4.31), Variable("X"), RealNumber(1)) + Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(5.31 * X^1.0)"), res)
  }

  test("Indeterminate^0 + Variable") {
    val res = (Indeterminate(RealNumber(4.31), Variable("X"), RealNumber(0)) + RealNumber(2)).optimize
    assert(res.isInstanceOf[RealNumber])
    assert(res.evaluate == 6.31)
  }

  test("Indeterminate^2 + Indeterminate^1") {
    val res = Indeterminate(RealNumber(4.31), Variable("X"), RealNumber(3)) +
      Indeterminate(RealNumber(31), Variable("X"), RealNumber(321))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("(4.31 * X^3.0) + (31.0 * X^321.0)"), res)
  }

  test("Indeterminate^1 + Indeterminate^1") {
    val res = Indeterminate(RealNumber(2), Variable("X"), RealNumber(3)) +
      Indeterminate(RealNumber(5), Variable("X"), RealNumber(3))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(7.0 * X^3.0)"), res)
  }

  test("Indeterminate + OtherIndeterminate") {
    assertThrows[EvaluateException] {
      Indeterminate(RealNumber(2), Variable("X"), RealNumber(3)) +
        Indeterminate(RealNumber(5), Variable("Y"), RealNumber(3))
    }
  }

  test("Indeterminate - RealNumber") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) - RealNumber(0.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("(4.0 * X^13.0) - 0.5"), res.toString)
  }

  test("Indeterminate - RealNumber = 0") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) - RealNumber(0.0)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(4.0 * X^13.0)"), res.toString)
  }

  test("Indeterminate - Variable") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) - Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("(4.0 * X^13.0) - X"), res)
  }

  test("Indeterminate - OtherVariable") {
    assertThrows[EvaluateException] {
      Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) - Variable("Y")
    }
  }

  test("Indeterminate - Variable - Degree == 1") {
    val res = Indeterminate(RealNumber(4), Variable("X"), RealNumber(1)) - Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(3.0 * X^1.0)"), res)
  }

  test("Indeterminate^2 - Indeterminate^1") {
    val res = Indeterminate(RealNumber(4.31), Variable("X"), RealNumber(3)) -
      Indeterminate(RealNumber(31), Variable("X"), RealNumber(321))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("(4.31 * X^3.0) - (31.0 * X^321.0)"), res)
  }

  test("Indeterminate^1 - Indeterminate^1") {
    val res = Indeterminate(RealNumber(2), Variable("X"), RealNumber(3)) -
      Indeterminate(RealNumber(5), Variable("X"), RealNumber(3))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(-3.0 * X^3.0)"), res)
  }

  test("Indeterminate - OtherIndeterminate") {
    assertThrows[EvaluateException] {
      Indeterminate(RealNumber(2), Variable("X"), RealNumber(3)) -
        Indeterminate(RealNumber(5), Variable("Y"), RealNumber(3))
    }
  }

  test("Indeterminate * RealNumber") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(13)) * RealNumber(0.5)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(2.0 * X^13.0)"), res)
  }

  test("Indeterminate * Variable") {
    val res = Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(3.5)) * Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("(4.0 * X^4.5)"), res)
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
