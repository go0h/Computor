package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException
import org.scalatest.funsuite.AnyFunSuite

class VariableTest extends AnyFunSuite {

  import fr.fourtytwo.expression.Implicit._


  ////////////////////////////////////////
  ///////////// PLUS METHODS /////////////
  ////////////////////////////////////////
  test("Variable + RealNumber") {
    val res = Variable("X") + RealNumber(0.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X + 0.5"))
  }

  test("-Variable + RealNumber") {
    val res = Variable("-X") + RealNumber(0.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("-X + 0.5"))
  }

  test("Variable + RealNumber = 0") {
    val res = Variable("X") + RealNumber(0)
    assert(res.isInstanceOf[Variable])
    assert(res.toString.equals("X"))
  }

  test("-Variable + RealNumber = 0") {
    val res = Variable("-X") + RealNumber(0)
    assert(res.isInstanceOf[Variable])
    assert(res.toString.equals("-X"))
  }

  test("Variable + Variable") {
    val res = Variable("X") + Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("2.0 * X"), res)
  }

  test("Variable + -Variable") {
    val res = Variable("X") + Variable("-X")
    assert(res.isInstanceOf[RealNumber])
    assert(res.toString.equals("0.0"), res)
  }

  test("-Variable + -Variable") {
    val res = Variable("-X") + Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-2.0 * X"), res)
  }

  test("-Variable + Variable") {
    val res = Variable("-X") + Variable("X")
    assert(res.isInstanceOf[RealNumber])
    assert(res.toString.equals("0.0"), res)
  }

  test("Variable + OtherVariable") {
    val res = Variable("X") + Variable("Y")
    assert(res.isInstanceOf[Operator], res)
  }

  test("Variable + Indeterminate") {
    val res = Variable("X") + Indeterminate(4, "X", 13)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X + 4.0 * X^13.0"), res)
  }

  test("-Variable + Indeterminate") {
    val res = Variable("-X") + Indeterminate(4, "X", 13)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("-X + 4.0 * X^13.0"), res)
  }

  test("Variable + Indeterminate. Degree == 1") {
    val res = Variable("X") + Indeterminate(4, "X", 1)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("5.0 * X"), res)
  }

  test("-Variable + Indeterminate. Degree == 1") {
    val res = Variable("-X") + Indeterminate(4, "X", 1)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("3.0 * X"), res)
  }

  test("Variable + OtherIndeterminate") {
    val res = Variable("X") + Indeterminate(4.0, "Y", 1)
    assert(res.isInstanceOf[Operator])
  }

  test("Variable + ComplexNumber") {
    val res = Variable("X") + ComplexNumber(4, 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X + 4.0 + 3.0i"), res)
  }

  test("Variable + ComplexNumber - 1") {
    val res = (Variable("X") + ComplexNumber(3, 0)).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X + 3.0"), res)
  }

  test("Variable + Matrix") {
    val res = Variable("X") + Matrix("[[1,2];[3,4]]")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X + [ 1.0, 2.0 ]\n[ 3.0, 4.0 ]"), res)
  }

  ////////////////////////////////////////
  ///////////// MINUS METHODS ////////////
  ////////////////////////////////////////
  test("Variable - RealNumber") {
    val res = Variable("X") - RealNumber(0.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X - 0.5"))
  }

  test("-Variable - RealNumber") {
    val res = Variable("-X") - RealNumber(0.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("-X - 0.5"))
  }

  test("Variable - RealNumber = 0") {
    val res = Variable("X") - RealNumber(0)
    assert(res.isInstanceOf[Variable])
    assert(res.toString.equals("X"))
  }

  test("Variable - Variable") {
    val res = (Variable("X") - Variable("X")).asInstanceOf[RealNumber]
    assert(res.isInstanceOf[RealNumber])
    assert(res == 0)
  }

  test("-Variable - Variable") {
    val res = Variable("-X") - Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-2.0 * X"), res)
  }

  test("-Variable - -Variable") {
    val res = (Variable("-X") - Variable("-X")).asInstanceOf[RealNumber]
    assert(res.isInstanceOf[RealNumber])
    assert(res == 0)
  }

  test("Variable - -Variable") {
    val res = Variable("X") - Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("2.0 * X"), res)
  }

  test("Variable - OtherVariable") {

    val res = Variable("X") - Variable("Y")
    assert(res.isInstanceOf[Operator])
  }

  test("Variable - OtherIndeterminate") {
    val res = Variable("X") - Indeterminate(4, "Y", 13)
    assert(res.isInstanceOf[Operator])
  }

  test("Variable - Indeterminate") {
    val res = Variable("X") - Indeterminate(4, "X", 13)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X - 4.0 * X^13.0"))
  }

  test("-Variable - Indeterminate") {
    val res = Variable("-X") - Indeterminate(4, "X", 13)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("-X - 4.0 * X^13.0"))
  }

  test("Variable - Indeterminate^1") {
    val res = Variable("X") - Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-3.0 * X"), res)
  }

  test("-Variable - Indeterminate^1") {
    val res = Variable("-X") - Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(1))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-5.0 * X"), res)
  }

  test("Variable - -Indeterminate^1") {
    val res = Variable("X") - Indeterminate(RealNumber(-4.0), Variable("X"), RealNumber(1))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("5.0 * X"), res)
  }

  test("-Variable - -Indeterminate^1") {
    val res = Variable("-X") - Indeterminate(RealNumber(-4.0), Variable("X"), RealNumber(1))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("3.0 * X"), res)
  }

  test("Variable - ComplexNumber") {
    val res = Variable("X") - ComplexNumber(4, 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X - 4.0 + 3.0i"), res)
  }

  test("Variable - ComplexNumber - 1") {
    val res = (Variable("X") - ComplexNumber(4, 0)).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X - 4.0"), res)
  }

  test("Variable - Matrix") {
    val res = Variable("X") - Matrix("[[1,2];[3,4]]")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X - [ 1.0, 2.0 ]\n[ 3.0, 4.0 ]"), res)
  }


  ////////////////////////////////////////
  /////////// MULTIPLY METHODS ///////////
  ////////////////////////////////////////
  test("Variable * RealNumber") {
    val res = Variable("X") * RealNumber(0.5)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("0.5 * X"))
  }

  test("-Variable * RealNumber") {
    val res = Variable("-X") * RealNumber(0.5)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-0.5 * X"), res)
  }

  test("Variable * Variable") {
    val res = Variable("X") * Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("X^2.0"))
  }

  test("-Variable * Variable") {
    val res = Variable("-X") * Variable("X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-1.0 * X^2.0"))
  }

  test("-Variable * -Variable") {
    val res = Variable("-X") * Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("X^2.0"))
  }

  test("Variable * -Variable") {
    val res = Variable("X") * Variable("-X")
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-1.0 * X^2.0"))
  }

  test("Variable * OtherVariable") {
    val res = Variable("X") * Variable("Y")
    assert(res.isInstanceOf[Operator])
  }

  test("Variable * Indeterminate") {
    val res = Variable("X") * Indeterminate(RealNumber(4), Variable("X"), RealNumber(13))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4.0 * X^14.0"), res.toString)
  }

  test("-Variable * Indeterminate") {
    val res = Variable("-X") * Indeterminate(RealNumber(4), Variable("X"), RealNumber(13))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-4.0 * X^14.0"), res.toString)
  }

  test("-Variable * -Indeterminate") {
    val res = Variable("-X") * Indeterminate(RealNumber(-4), Variable("X"), RealNumber(13))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4.0 * X^14.0"), res)
  }

  test("Variable * ComplexNumber") {
    val res = Variable("X") * ComplexNumber(4, 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X * 4.0 + 3.0i"), res)
  }

  test("Variable * ComplexNumber - 1") {
    val res = (Variable("X") * ComplexNumber(4, 0)).evaluate
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4.0 * X"), res)
  }

  test("Variable * Matrix") {
    val res = Variable("X") * Matrix("[[1,2];[3,4]]")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X * [ 1.0, 2.0 ]\n[ 3.0, 4.0 ]"), res)
  }

  ////////////////////////////////////////
  /////////// DIVISION METHODS ///////////
  ////////////////////////////////////////
  test("Variable / RealNumber") {
    val res = Variable("X") / RealNumber(4)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("0.25 * X"))
  }

  test("-Variable / RealNumber") {
    val res = Variable("-X") / RealNumber(4)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-0.25 * X"))
  }

  test("Variable / Variable") {
    val res = (Variable("X") / Variable("X")).asInstanceOf[RealNumber]
    assert(res.isInstanceOf[RealNumber])
    assert(res == 1.0)
  }

  test("-Variable / Variable") {
    val res = (Variable("-X") / Variable("X")).asInstanceOf[RealNumber]
    assert(res.isInstanceOf[RealNumber])
    assert(res == -1.0)
  }

  test("-Variable / -Variable") {
    val res = (Variable("-X") / Variable("-X")).asInstanceOf[RealNumber]
    assert(res.isInstanceOf[RealNumber])
    assert(res == 1.0)
  }

  test("Variable / -Variable") {
    val res = (Variable("X") / Variable("-X")).asInstanceOf[RealNumber]
    assert(res.isInstanceOf[RealNumber])
    assert(res == -1.0)
  }

  test("Variable / OtherVariable") {
    val res = Variable("X") / Variable("Y")
    assert(res.isInstanceOf[Operator])
  }

  test("Variable / Indeterminate") {
    val res = Variable("X") / Indeterminate(4, "X", 0.112)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("0.25 * X^0.888"), res.toString)
  }

  test("-Variable / Indeterminate") {
    val res = Variable("-X") / Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(0.112))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("-0.25 * X^0.888"), res.toString)
  }

  test("-Variable / -Indeterminate") {
    val res = Variable("-X") / Indeterminate(RealNumber(-4.0), Variable("X"), RealNumber(0.112))
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("0.25 * X^0.888"), res.toString)
  }

  test("Variable / OtherIndeterminate") {
    val res = Variable("X") / Indeterminate(4, "Y", 13)
    assert(res.isInstanceOf[Operator])
  }

  test("Variable / ComplexNumber") {
    val res = Variable("X") / ComplexNumber(4, 23)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X / 4.0 + 23.0i"), res)
  }

  test("Variable / Matrix") {
    assertThrows[EvaluateException] {
      Variable("X") / Matrix("[[1,2];[3,4]]")
    }
  }

  ////////////////////////////////////////
  ///////////// POWER METHODS ////////////
  ////////////////////////////////////////
  test("Variable ^ RealNumber") {
    val res = Variable("X") ^ RealNumber(4)
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("X^4.0"), res)
  }

  test("Variable ^ Variable") {
    val res = Variable("X") ^ Variable("A")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X ^ A"), res)
  }

  test("Variable ^ Indeterminate") {
    val res = Variable("X") ^ new Indeterminate(1, "Y", 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X ^ Y^3.0"), res)
  }

  test("Variable ^ ComplexNumber") {
    assertThrows[EvaluateException] {
      Variable("X") ^ ComplexNumber(3, 2)
    }
  }

  test("Variable ^ Matrix") {
    assertThrows[EvaluateException] {
      Variable("X") ^ Matrix("[[1,2];[3,4]]")
    }
  }

  ////////////////////////////////////////
  ///////////// MODULO METHODS ///////////
  ////////////////////////////////////////
  test("Variable % RealNumber") {
    val res = Variable("X") % RealNumber(4)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X % 4.0"), res)
  }

  test("Variable % Variable") {
    val res = Variable("X") % Variable("A")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X % A"), res)
  }

  test("Variable % Indeterminate") {
    val res = Variable("X") % new Indeterminate(1, "Y", 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("X % Y^3.0"), res)
  }

  test("Variable % ComplexNumber") {
    assertThrows[EvaluateException] {
      Variable("X") % ComplexNumber(3, 2)
    }
  }

  test("Variable % Matrix") {
    assertThrows[EvaluateException] {
      Variable("X") % Matrix("[[1,2];[3,4]]")
    }
  }
}
