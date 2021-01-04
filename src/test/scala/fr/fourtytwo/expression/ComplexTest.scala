package fr.fourtytwo.expression

import fr.fourtytwo.computor.{Computor, TOKENIZER}
import fr.fourtytwo.exception.EvaluateException
import org.scalatest.funsuite.AnyFunSuite
import fr.fourtytwo.math._


class ComplexTest extends AnyFunSuite {

  test("Parsing test") {

    val expressions = Array(
      "2i + 3", "2i - 3",
      "2 * i + 3", "2 * i - 3",
      "-2 * i - 3", "2 * ((((i)))) - 33",
      "i * 3 + 3", "3 - -i*3")

    for (expr <- expressions) {

      val tokens = TOKENIZER.generateTokens(expr)
      val complex = Computor(tokens).solve.evaluate

      assert(complex.getType.equals("ComplexNumber"), complex.getType )
    }

  }

  import fr.fourtytwo.expression.Implicit._

  ////////////////////////////////////////
  ///////////// PLUS METHODS /////////////
  ////////////////////////////////////////
  test("ComplexNumber + RealNumber") {
    val res = ComplexNumber(8, 2) + RealNumber(4)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("12.0 + 2.0i"), res)
  }

  test("ComplexNumber + RealNumber = 0") {
    val res = ComplexNumber(8, -2) + RealNumber(0)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("8.0 - 2.0i"), res)
  }

  test("ComplexNumber + Variable") {
    val res = ComplexNumber(8, -2) + Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("8.0 - 2.0i + X"), res)
  }

  test("ComplexNumber + Variable - 1") {
    val res = (ComplexNumber(3, 0) + Variable("X")).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3.0 + X"), res)
  }

  test("ComplexNumber + Indeterminate") {
    val res = ComplexNumber(4, 2) + Indeterminate(2, "X", 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 2.0i + 2.0 * X^3.0"), res)
  }

  test("ComplexNumber + Indeterminate = 1") {
    val res = (ComplexNumber(4, 0) + Indeterminate(2, "X", 3)).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 2.0 * X^3.0"), res)
  }

  test("ComplexNumber + ComplexNumber") {
    val res = (ComplexNumber(4, 3) + ComplexNumber(4, 6)).evaluate
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("8.0 + 9.0i"), res)
  }

  test("ComplexNumber + ComplexNumber = RealNumber") {
    val res = (ComplexNumber(4, 3) + ComplexNumber(4, -3)).evaluate
    assert(res.isInstanceOf[RealNumber])
    assert(res.toString.equals("8.0"), res)
  }

  test("ComplexNumber + Matrix") {
    assertThrows[EvaluateException] {
      ComplexNumber(4, 3) + Matrix("[[1,2];[3,4]]")
    }
  }


  ////////////////////////////////////////
  ///////////// MINUS METHODS ////////////
  ////////////////////////////////////////
  test("ComplexNumber - RealNumber") {
    val res = ComplexNumber(8, 2) - RealNumber(4)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("4.0 + 2.0i"), res)
  }

  test("ComplexNumber - RealNumber = 0") {
    val res = RealNumber(0) - ComplexNumber(8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("-8.0 - 2.0i"), res)
  }

  test("ComplexNumber - Variable") {
    val res = ComplexNumber(4, 3) - Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 3.0i - X"), res)
  }

  test("ComplexNumber - Variable - 1") {
    val res = (ComplexNumber(4, 0) - Variable("X")).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 - X"), res)
  }

  test("ComplexNumber - Indeterminate") {
    val res = ComplexNumber(4, 2) - Indeterminate(2, "X", 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 2.0i - 2.0 * X^3.0"), res)
  }

  test("ComplexNumber - Indeterminate - 1") {
    val res = (ComplexNumber(4, 0) - Indeterminate(2, "X", 3)).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 - 2.0 * X^3.0"), res)
  }

  test("ComplexNumber - ComplexNumber") {
    val res = ComplexNumber(4, 3) - ComplexNumber(3, 1)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("1.0 + 2.0i"), res)
  }

  test("ComplexNumber - ComplexNumber = RealNumber") {
    val res = (ComplexNumber(4, 3) - ComplexNumber(-4, 3)).evaluate
    assert(res.isInstanceOf[RealNumber])
    assert(res.toString.equals("8.0"), res)
  }

  test("ComplexNumber - Matrix") {
    assertThrows[EvaluateException] {
      ComplexNumber(4, 3) - Matrix("[[1,2];[3,4]]")
    }
  }

  ////////////////////////////////////////
  /////////// MULTIPLY METHODS ///////////
  ////////////////////////////////////////
  test("ComplexNumber * RealNumber") {
    val res = ComplexNumber(8, 2) * RealNumber(4)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("32.0 + 8.0i"), res)
  }

  test("-ComplexNumber * RealNumber") {
    val res = ComplexNumber(-8, 2) * RealNumber(4)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("-32.0 + 8.0i"), res)
  }

  test("ComplexNumber * Variable") {
    val res = ComplexNumber(4, 3) * Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 3.0i * X"), res)
  }

  test("ComplexNumber * Variable - 1") {
    val res = (ComplexNumber(4, 0) * Variable("X")).evaluate
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4.0 * X"), res)
  }

  test("ComplexNumber * Indeterminate") {
    val res = ComplexNumber(4, 3) * Indeterminate(4, "X", 3.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 3.0i * 4.0 * X^3.5"), res)
  }

  test("ComplexNumber * Indeterminate - 1") {
    val res = (ComplexNumber(4, 0) * Indeterminate(4.0, "X", 3.5)).evaluate
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("16.0 * X^3.5"), res)
  }

  test("ComplexNumber * ComplexNumber") {
    val res = (ComplexNumber(4, 2) * ComplexNumber(4, 3)).evaluate
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("10.0 + 20.0i"), res)
  }

  test("ComplexNumber * ComplexNumber - 1") {
    val res = (ComplexNumber(4, 2) * ComplexNumber(4, -3)).evaluate
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("22.0 - 4.0i"), res)
  }

  test("ComplexNumber * Matrix ") {
    assertThrows[EvaluateException] {
      (ComplexNumber(4, 2) * Matrix("[[1,2];[3,4]]")).evaluate
    }
  }


  ////////////////////////////////////////
  /////////// DIVISION METHODS ///////////
  ////////////////////////////////////////
  test("ComplexNumber / RealNumber") {
    val res = ComplexNumber(2, 4) / RealNumber(4)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("0.5 + 1.0i"), res)
  }

  test("ComplexNumber / Variable") {
    val res = ComplexNumber(4, 23) / Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 23.0i / X"), res)
  }

  test("ComplexNumber / Indeterminate") {
    val res = ComplexNumber(4, 45) / Indeterminate(4.0, "X", 2)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 45.0i / 4.0 * X^2.0"), res)
  }

  test("ComplexNumber / Indeterminate - 1") {
    val res = (ComplexNumber(16, 0) / Indeterminate(4.0, "X", 2)).evaluate
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4.0 * X^-2.0"), res)
  }

  test("ComplexNumber / ComplexNumber") {
    val res = (ComplexNumber(4, 2) / ComplexNumber(4, 3)).evaluate
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("0.88 - 0.16i"), res)
  }

  test("ComplexNumber / ComplexNumber - 1") {
    val res = (ComplexNumber(4, 2) / ComplexNumber(4, -3)).evaluate
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("0.4 + 0.8i"), res)
  }

  test("ComplexNumber / Matrix ") {
    assertThrows[EvaluateException] {
      (ComplexNumber(4, 2) / Matrix("[[1,2];[3,4]]")).evaluate
    }
  }

  ////////////////////////////////////////
  ///////////// POWER METHODS ////////////
  ////////////////////////////////////////
  test("ComplexNumber ^ RealNumber") {
    val res = ComplexNumber(1, sqrt(3)) ^ RealNumber(3)
    assert(res.isInstanceOf[RealNumber])
    assert(res.asInstanceOf[RealNumber].getNum.ceil == -8)
  }

  test("ComplexNumber ^ RealNumber - 1") {
    val res = ComplexNumber(4, 3.5) ^ RealNumber(5)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.asInstanceOf[ComplexNumber]
      .toString.equals("-3814.7500000000005 - 1854.7812500000007i"), res)
  }

  test("ComplexNumber ^ Variable") {
    val res = ComplexNumber(2, 4) ^ Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2.0 + 4.0i ^ X"))
  }

  test("ComplexNumber ^ Indeterminate") {
    val res = ComplexNumber(2, 4) ^ Indeterminate(4, "X", 4)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2.0 + 4.0i ^ 4.0 * X^4.0"))
  }

  test("ComplexNumber ^ ComplexNumber") {
    assertThrows[EvaluateException] {
      ComplexNumber(2, 4) ^ ComplexNumber(4, 3)
    }
  }

  test("ComplexNumber ^ Matrix") {
    assertThrows[EvaluateException] {
      ComplexNumber(2, 4) ^ Matrix("[[1,2];[3,4]]")
    }
  }

  ////////////////////////////////////////
  ///////////// MODULO METHODS ///////////
  ////////////////////////////////////////
  test("ComplexNumber % RealNumber") {
    assertThrows[EvaluateException] {
      ComplexNumber(2, 4) % RealNumber(4)
    }
  }

  test("ComplexNumber % Variable") {
    assertThrows[EvaluateException] {
      ComplexNumber(2, 4) % Variable("X")
    }
  }

  test("ComplexNumber % Indeterminate") {
    assertThrows[EvaluateException] {
      ComplexNumber(2, 4) % Indeterminate(4, "X", 4)
    }
  }

  test("ComplexNumber % ComplexNumber") {
    assertThrows[EvaluateException] {
      ComplexNumber(2, 4) % ComplexNumber(4, 3)
    }
  }

  test("ComplexNumber % Matrix") {
    assertThrows[EvaluateException] {
      ComplexNumber(2, 4) % Matrix("[[1,2];[3,4]]")
    }
  }

}
