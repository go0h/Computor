package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException
import org.scalatest.funsuite.AnyFunSuite

class RealNumberTest extends AnyFunSuite {

  ////////////////////////////////////////
  ///////////// PLUS METHODS /////////////
  ////////////////////////////////////////
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
    val res = RealNumber(3.14) + Indeterminate(RealNumber(4), Variable("X"), RealNumber(1))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3.14 + 4 * X"), res)
  }

  test("RealNumber = 0 + Indeterminate") {
    val res = RealNumber(0) + Indeterminate(RealNumber(4), Variable("X"), RealNumber(1))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4 * X"), res)
  }

  test("RealNumber + ComplexNumber") {
    val res = RealNumber(4) + ComplexNumber(8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("12 + 2i"), res)
  }

  test("RealNumber = 0 + ComplexNumber") {
    val res = RealNumber(0) + ComplexNumber(8, -2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("8 - 2i"), res)
  }

  test("RealNumber + Matrix") {
    assertThrows[EvaluateException] {
      RealNumber(32) + Matrix("[[1,2];[3,4]]")
    }
  }


  ////////////////////////////////////////
  ///////////// MINUS METHODS ////////////
  ////////////////////////////////////////
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
    val res = RealNumber(3.14) - Indeterminate(RealNumber(4), Variable("X"), RealNumber(1))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3.14 - 4 * X"), res)
  }

  test("RealNumber - ComplexNumber") {
    val res = RealNumber(4) - ComplexNumber(8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("-4 - 2i"), res)
  }

  test("RealNumber = 0 - ComplexNumber") {
    val res = RealNumber(0) - ComplexNumber(8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("-8 - 2i"), res)
  }

  test("RealNumber - Matrix") {
    assertThrows[EvaluateException] {
      RealNumber(32) - Matrix("[[1,2];[3,4]]")
    }
  }


  ////////////////////////////////////////
  /////////// MULTIPLY METHODS ///////////
  ////////////////////////////////////////
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
    val res = RealNumber(0.5) * Indeterminate(RealNumber(4), Variable("X"), RealNumber(132.22))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("2 * X^132.22"))
  }

  test("RealNumber * Indeterminate - 1") {
    val res = RealNumber(0.25) * Indeterminate(RealNumber(4), Variable("X"), RealNumber(-12))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("X^-12"))
  }

  test("RealNumber * ComplexNumber") {
    val res = RealNumber(4) * ComplexNumber(8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("32 + 8i"), res)
  }

  test("RealNumber * -ComplexNumber") {
    val res = RealNumber(4) * ComplexNumber(-8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("-32 + 8i"), res)
  }

  test("RealNumber * Matrix") {
    val expr1 = "[ [1, 7]; [0, 2.5]]"
    val expr2 = "[ [2, 14]; [0, 5]]"
    assert((RealNumber(2) * Matrix(expr1)).equals(Matrix(expr2)))
  }


  ////////////////////////////////////////
  /////////// DIVISION METHODS ///////////
  ////////////////////////////////////////
  test("RealNumber / RealNumber") {
    val res = RealNumber(10) / RealNumber(4)
    assert(res == 10.0 / 4)
  }

  test("RealNumber / Variable") {
    val res = RealNumber(10) / Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("10 * X^-1"))
  }

  test("RealNumber / -Variable") {
    val res = RealNumber(10) / Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-10 * X^-1"))
  }

  test("RealNumber / Indeterminate") {
    val res = RealNumber(10) / Indeterminate(RealNumber(2), Variable("X"), RealNumber(4))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("5 * X^-4"))
  }

  test("RealNumber / ComplexNumber") {
    val res = RealNumber(4) / ComplexNumber(2, 4)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("0.4 - 0.8i"), res)
  }

  test("RealNumber / Matrix") {
    assertThrows[EvaluateException] {
      RealNumber(4) / Matrix("[ [1, 7]; [0, 2.5]]")
    }
  }

  ////////////////////////////////////////
  ///////////// POWER METHODS ////////////
  ////////////////////////////////////////
  test("RealNumber ^ RealNumber") {
    val res = RealNumber(2) ^ RealNumber(4)
    assert(res.isInstanceOf[RealNumber])
    assert(res.asInstanceOf[RealNumber] == 16, res)
  }

  test("RealNumber ^ Variable") {
    val res = RealNumber(2) ^ Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 ^ X"), res)
  }

  test("RealNumber ^ Indeterminate") {
    val res = RealNumber(2) ^ new Indeterminate(2, "X", 2)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 ^ 2 * X^2"), res)
  }

  test("RealNumber ^ ComplexNumber") {
    assertThrows[EvaluateException] {
      RealNumber(2) ^ ComplexNumber(2, 2)
    }
  }

  test("RealNumber ^ Matrix") {
    assertThrows[EvaluateException] {
      RealNumber(2) ^ Matrix("[[1,2];[3,4]]")
    }
  }

  ////////////////////////////////////////
  ///////////// MODULO METHODS ///////////
  ////////////////////////////////////////
  test("RealNumber % RealNumber") {
    val res = RealNumber(5) % RealNumber(4)
    assert(res.isInstanceOf[RealNumber])
    assert(res.asInstanceOf[RealNumber] == 1, res)
  }

  test("RealNumber % Variable") {
    val res = RealNumber(2) % Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 % X"), res)
  }

  test("RealNumber % Indeterminate") {
    val res = RealNumber(2) % new Indeterminate(2, "X", 2)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 % 2 * X^2"), res)
  }

  test("RealNumber % ComplexNumber") {
    assertThrows[EvaluateException] {
      RealNumber(2) % ComplexNumber(2, 2)
    }
  }

  test("RealNumber % Matrix") {
    assertThrows[EvaluateException] {
      RealNumber(2) % Matrix("[[1,2];[3,4]]")
    }
  }


  ////////////////////////////////////////
  ////////// TERM-BY-TERM MATRIX /////////
  ///////////// MULTIPLICATION ///////////
  ////////////////////////////////////////
  test("RealNumber ** RealNumber") {
    assertThrows[EvaluateException] {
      RealNumber(2) ** RealNumber(3)
    }
  }

  test("RealNumber ** Matrix") {
    assertThrows[EvaluateException] {
      RealNumber(2) ** Matrix("[[1,2];[3,4]]")
    }
  }

}
