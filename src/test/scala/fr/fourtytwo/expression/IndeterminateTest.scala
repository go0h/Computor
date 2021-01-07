package fr.fourtytwo.expression

import org.scalatest.funsuite.AnyFunSuite
import fr.fourtytwo.exception.{EvaluateException, ParseException}


class IndeterminateTest extends AnyFunSuite {

  test("Basic with Integers") {
    val indeterminate = Indeterminate("4*X^2")
    assert(indeterminate.toString.equals("4 * X^2"))
  }

  test("Basic with minus") {
    val indeterminate = Indeterminate("-4*X^2")
    assert(indeterminate.toString.equals("-4 * X^2"))
  }

  test("Basic with Double") {
    val indeterminate = Indeterminate("-4.00*X^2.00")
    assert(indeterminate.toString.equals("-4 * X^2"))
  }

  test("Basic with minus Double") {
    val indeterminate = Indeterminate("-4.123*X^2.87654")
    assert(indeterminate.toString.equals("-4.123 * X^2.87654"))
  }

  test("Zeros first") {
    val indeterminate = Indeterminate("000.123*X^000.87654")
    assert(indeterminate.toString.equals("0.123 * X^0.87654"))
  }

  test("Minus zeros first") {
    val indeterminate = Indeterminate("-0.123*X^0.87654")
    assert(indeterminate.toString.equals("-0.123 * X^0.87654"))
  }

  test("Only zeros") {
    val indeterminate = Indeterminate("-0*X^0")
    assert(indeterminate.toString.equals("0 * X^0"), indeterminate)
  }

  test("Variable name length") {
    val indeterminate = Indeterminate("1*XYZ^1")

    assert(indeterminate.toString.equals("XYZ"))
    assert(indeterminate.variable.toString.equals("XYZ"))
  }

  test("Minus after constant") {
    assertThrows[ParseException] {
      Indeterminate("1-*XYZ^1")
    }
  }

  test("Minus after degree") {
    assertThrows[ParseException] {
      Indeterminate("1*XYZ^1-")
    }
  }

  test("Reverse with Integers") {
    val indeterminate = Indeterminate("X^2*4")
    assert(indeterminate.toString.equals("4 * X^2"))
  }

  test("Reverse with minus") {
    val indeterminate = Indeterminate("X^2*-4")
    assert(indeterminate.toString.equals("-4 * X^2"))
  }

  test("Reverse with minus double") {
    val indeterminate = Indeterminate("X^2*-401")
    assert(indeterminate.toString.equals("-401 * X^2"))
  }

  test("Start with letter") {
    assertThrows[ParseException] {
      Indeterminate("a1*XYZ^1-")
    }
  }

  test("Ends with letter") {
    assertThrows[ParseException] {
      Indeterminate("a1*XYZ^1asd")
    }
  }

  test("Bad constant") {
    assertThrows[ParseException] {
      Indeterminate("1.0.0*XYZ^1.0")
    }
  }

  test("Bad degree") {
    assertThrows[ParseException] {
      Indeterminate("1*XYZ^10.")
    }
  }


  import fr.fourtytwo.expression.Implicit._

  ////////////////////////////////////////
  ///////////// PLUS METHODS /////////////
  ////////////////////////////////////////
  test("Indeterminate + RealNumber") {
    val res = Indeterminate(4, "X", 13) + RealNumber(0.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 * X^13 + 0.5"), res.toString)
  }

  test("Indeterminate + RealNumber = 0") {
    val res = Indeterminate(4, "X", 13) + RealNumber(0)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4 * X^13"), res.toString)
  }

  test("Indeterminate + Variable") {
    val res = Indeterminate(4, "X", 13) + Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 * X^13 + X"), res)
  }

  test("Indeterminate + -Variable") {
    val res = Indeterminate(4, "X", 13) + Variable("-X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 * X^13 + -X"), res)
  }

  test("Indeterminate + OtherVariable") {
    val res = Indeterminate(4, "X", 13) + Variable("Y")
    assert(res.isInstanceOf[Operator])
  }

  test("Indeterminate + Variable - Degree == 1") {
    val res = Indeterminate(4.31, "X", 1) + Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("5.31 * X"), res)
  }

  test("Indeterminate + -Variable - Degree == 1") {
    val res = Indeterminate(4.3, "X", 1) + Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("3.3 * X"), res)
  }

  test("Indeterminate^0 + Variable") {
    val res = Indeterminate(4.31, "X", 0) + Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.31 * X^0 + X"), res)
  }

  test("Indeterminate^0 + -Variable") {
    val res = Indeterminate(4.31, "X", 0) + Variable("-X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.31 * X^0 + -X"), res)
  }

  test("Indeterminate^2 + Indeterminate^1") {
    val res = Indeterminate(4.31, "X", 3) +
      Indeterminate(31, "X", 321)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.31 * X^3 + 31 * X^321"), res)
  }

  test("Indeterminate^1 + Indeterminate^1") {
    val res = Indeterminate(2, "X", RealNumber(3)) +
      Indeterminate(5, "X", RealNumber(3))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("7 * X^3"), res)
  }

  test("Indeterminate + OtherIndeterminate") {
    val res = Indeterminate(2, "X", 3) + Indeterminate(5, "Y", 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 * X^3 + 5 * Y^3"), res)
  }

  test("Indeterminate + ComplexNumber") {
    val res = Indeterminate(2, "X", 3) + ComplexNumber(4, 2)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 * X^3 + 4 + 2i"), res)
  }

  test("Indeterminate + ComplexNumber = 1") {
    val res = (Indeterminate(2, "X", 3) + ComplexNumber(4, 0)).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 * X^3 + 4"), res)
  }

  test("Indeterminate + Matrix") {
    assertThrows[EvaluateException] {
      Indeterminate(2, "X", 3) + Matrix("[[1,2];[3,4]]")
    }
  }


  ////////////////////////////////////////
  ///////////// MINUS METHODS ////////////
  ////////////////////////////////////////
  test("Indeterminate - RealNumber") {
    val res = Indeterminate(4, "X", 13) - RealNumber(0.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 * X^13 - 0.5"), res.toString)
  }

  test("Indeterminate - RealNumber = 0") {
    val res = Indeterminate(4, "X", 13) - RealNumber(0)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4 * X^13"), res.toString)
  }

  test("Indeterminate - Variable") {
    val res = Indeterminate(4, "X", 13) - Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 * X^13 - X"), res)
  }

  test("Indeterminate - -Variable") {
    val res = Indeterminate(4, "X", 13) - Variable("-X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 * X^13 - -X"), res)
  }

  test("Indeterminate - OtherVariable") {
    val res = Indeterminate(4, "X", 13) - Variable("Y")
    assert(res.isInstanceOf[Operator])
  }

  test("Indeterminate - Variable - Degree == 1") {
    val res = Indeterminate(RealNumber(4), "X", 1) - Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("3 * X"), res)
  }

  test("Indeterminate - -Variable - Degree == 1") {
    val res = Indeterminate(RealNumber(4), "X", 1) - Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("5 * X"), res)
  }

  test("Indeterminate^2 - Indeterminate^1") {
    val res = Indeterminate(RealNumber(4.31), "X", 3) -
      Indeterminate(RealNumber(31), "X", RealNumber(321))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.31 * X^3 - 31 * X^321"), res)
  }

  test("Indeterminate^1 - Indeterminate^1") {
    val res = Indeterminate(2, "X", 3) -
      Indeterminate(5, "X", 3)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-3 * X^3"), res)
  }

  test("Indeterminate - OtherIndeterminate") {
    val res = Indeterminate(2, "X", 3) - Indeterminate(5, "Y", 3)
    assert(res.isInstanceOf[Operator])
  }

  test("Indeterminate - ComplexNumber") {
    val res = Indeterminate(2, "X", 3) - ComplexNumber(4, 2)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 * X^3 - 4 + 2i"), res)
  }

  test("Indeterminate - ComplexNumber - 1") {
    val res = (Indeterminate(2, "X", 3) - ComplexNumber(4, 0)).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 * X^3 - 4"), res)
  }

  test("Indeterminate - Matrix") {
    assertThrows[EvaluateException] {
      Indeterminate(2, "X", 3) - Matrix("[[1,2];[3,4]]")
    }
  }


  ////////////////////////////////////////
  /////////// MULTIPLY METHODS ///////////
  ////////////////////////////////////////
  test("Indeterminate * RealNumber") {
    val res = Indeterminate(4, "X", 13) * RealNumber(0.5)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("2 * X^13"), res)
  }

  test("Indeterminate * Variable") {
    val res = Indeterminate(4, "X", 3.5) * Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4 * X^4.5"), res)
  }

  test("Indeterminate * -Variable") {
    val res = Indeterminate(4, "X", 3.5) * Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-4 * X^4.5"), res)
  }

  test("-Indeterminate * -Variable") {
    val res = Indeterminate(RealNumber(-4), "X", 3.5) * Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4 * X^4.5"), res)
  }


  test("Indeterminate * OtherVariable") {
    val res = Indeterminate(4, "X", 3.5) * Variable("Y")
    assert(res.isInstanceOf[Operator])
  }

  test("Indeterminate * Indeterminate") {
    val res = Indeterminate(4, "X", 3.5) *
      Indeterminate(5, "X", 3.5)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("20 * X^7"), res.toString)
  }

  test("Indeterminate * OtherIndeterminate") {
    val res = Indeterminate(4, "X", 3.5) * Indeterminate(5, "V", 3.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 * X^3.5 * 5 * V^3.5"), res.toString)
  }

  test("Indeterminate * ComplexNumber") {
    val res = Indeterminate(4, "X", 3.5) * ComplexNumber(4, 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 * X^3.5 * 4 + 3i"), res)
  }

  test("Indeterminate * ComplexNumber - 1") {
    val res = (Indeterminate(4, "X", 3.5) * ComplexNumber(4, 0)).evaluate
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("16 * X^3.5"), res)
  }

  test("Indeterminate * Matrix") {
    val res = Indeterminate(2, "X", 3) * Matrix("[[1,2];[3,4]]")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 * X^3 * [ 1, 2 ]\n[ 3, 4 ]"))
  }


  ////////////////////////////////////////
  /////////// DIVISION METHODS ///////////
  ////////////////////////////////////////
  test("Indeterminate / RealNumber") {
    val res = Indeterminate(4, "X", 13) / RealNumber(0.5)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("8 * X^13"), res)
  }

  test("Indeterminate / RealNumber = 0") {
    assertThrows[ArithmeticException] {
      Indeterminate(4, "X", 13) / RealNumber(0)
    }
  }

  test("Indeterminate / Variable") {
    val res = Indeterminate(4, "X", 13) / Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4 * X^12"), res.toString)
  }

  test("Indeterminate / -Variable") {
    val res = Indeterminate(4, "X", 13) / Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-4 * X^12"), res.toString)
  }

  test("Indeterminate / Variable = Zero") {
    val res = (Indeterminate(4, "X", 1) / Variable("X")).asInstanceOf[RealNumber]
    assert(res.isInstanceOf[RealNumber])
    assert(res == 4)
  }

  test("Indeterminate / -Variable = Zero") {
    val res = (Indeterminate(4, "X", 1) / Variable("-X")).asInstanceOf[RealNumber]
    assert(res.isInstanceOf[RealNumber])
    assert(res == -4)
  }

  test("Indeterminate / Indeterminate - equal") {
    val res = (Indeterminate(4, "X", 1) / Indeterminate(4, "X", 1)).asInstanceOf[RealNumber]
    assert(res.isInstanceOf[RealNumber])
    assert(res == 1)
  }

  test("Indeterminate / Indeterminate") {
    val res = Indeterminate(4, "X", 2) / Indeterminate(2, "X", 1)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("2 * X"), res.toString)
  }

  test("Indeterminate / ComplexNumber") {
    val res = Indeterminate(4, "X", 2) / ComplexNumber(4, 45)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 * X^2 / 4 + 45i"), res)
  }

  test("Indeterminate / ComplexNumber - 1") {
    val res = (Indeterminate(16, "X", 2) / ComplexNumber(4, 0)).evaluate
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4 * X^2"), res)
  }

  test("Indeterminate / Matrix") {
    assertThrows[EvaluateException] {
      Indeterminate(2, "X", 3) / Matrix("[[1,2];[3,4]]")
    }
  }

  ////////////////////////////////////////
  ///////////// POWER METHODS ////////////
  ////////////////////////////////////////
  test("Indeterminate ^ RealNumber") {
    val res = Indeterminate(2, "X", 3) ^ RealNumber(4)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("16 * X^12"), res)
  }

  test("Indeterminate ^ Variable") {
    val res = Indeterminate(2, "X", 3) ^ Variable("Y")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 * X^3 ^ Y"), res)
  }

  test("Indeterminate ^ Indeterminate") {
    val res = Indeterminate(2, "X", 3) ^ Indeterminate(2, "Y", 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 * X^3 ^ 2 * Y^3"), res)
  }

  test("Indeterminate ^ ComplexNumber") {
    assertThrows[EvaluateException]{
      Indeterminate(2, "X", 3) ^ ComplexNumber(2, 3)
    }
  }

  test("Indeterminate ^ Matrix") {
    assertThrows[EvaluateException]{
      Indeterminate(2, "X", 3) ^ Matrix("[[1,2];[3,4]]")
    }
  }

  ////////////////////////////////////////
  ///////////// MODULO METHODS ///////////
  ////////////////////////////////////////
  test("Indeterminate % RealNumber") {
    val res = Indeterminate(2, "X", 3) % RealNumber(4)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 * X^3 % 4"), res)
  }

  test("Indeterminate % Variable") {
    val res = Indeterminate(2, "X", 3) % Variable("Y")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 * X^3 % Y"), res)
  }

  test("Indeterminate % Indeterminate") {
    val res = Indeterminate(2, "X", 3) % Indeterminate(2, "Y", 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 * X^3 % 2 * Y^3"), res)
  }

  test("Indeterminate % ComplexNumber") {
    assertThrows[EvaluateException]{
      Indeterminate(2, "X", 3) % ComplexNumber(2, 3)
    }
  }

  test("Indeterminate % Matrix") {
    assertThrows[EvaluateException]{
      Indeterminate(2, "X", 3) % Matrix("[[1,2];[3,4]]")
    }
  }

}
