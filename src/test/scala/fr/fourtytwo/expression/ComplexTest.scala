package fr.fourtytwo.expression

import fr.fourtytwo.computor.{Computor, TOKENIZER}
import fr.fourtytwo.exception.EvaluateException
import org.scalatest.funsuite.AnyFunSuite
import fr.fourtytwo.math._


class ComplexTest extends AnyFunSuite {

  test("Parsing test") {

    val expressions = Array(
      "2i + 3.0", "2.0i - 3",
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
    assert(res.toString.equals("12 + 2i"), res)
  }

  test("ComplexNumber + RealNumber = 0") {
    val res = ComplexNumber(8, -2) + RealNumber(0)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("8 - 2i"), res)
  }

  test("ComplexNumber + Variable") {
    val res = ComplexNumber(8, -2) + Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("8 - 2i + X"), res)
  }

  test("ComplexNumber + Variable - 1") {
    val res = (ComplexNumber(3, 0) + Variable("X")).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3 + X"), res)
  }

  test("ComplexNumber + Indeterminate") {
    val res = ComplexNumber(4, 2) + Indeterminate(2, "X", 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 + 2i + 2 * X^3"), res)
  }

  test("ComplexNumber + Indeterminate = 1") {
    val res = (ComplexNumber(4, 0) + Indeterminate(2, "X", 3)).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 + 2 * X^3"), res)
  }

  test("ComplexNumber + ComplexNumber") {
    val res = (ComplexNumber(4, 3) + ComplexNumber(4, 6)).evaluate
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("8 + 9i"), res)
  }

  test("ComplexNumber + ComplexNumber = RealNumber") {
    val res = (ComplexNumber(4, 3) + ComplexNumber(4, -3)).evaluate
    assert(res.isInstanceOf[RealNumber])
    assert(res.toString.equals("8"), res)
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
    assert(res.toString.equals("4 + 2i"), res)
  }

  test("ComplexNumber - RealNumber = 0") {
    val res = RealNumber(0) - ComplexNumber(8, 2)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("-8 - 2i"), res)
  }

  test("ComplexNumber - Variable") {
    val res = ComplexNumber(4, 3) - Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 + 3i - X"), res)
  }

  test("ComplexNumber - Variable - 1") {
    val res = (ComplexNumber(4, 0) - Variable("X")).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 - X"), res)
  }

  test("ComplexNumber - Indeterminate") {
    val res = ComplexNumber(4, 2) - Indeterminate(2, "X", 3)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 + 2i - 2 * X^3"), res)
  }

  test("ComplexNumber - Indeterminate - 1") {
    val res = (ComplexNumber(4, 0) - Indeterminate(2, "X", 3)).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 - 2 * X^3"), res)
  }

  test("ComplexNumber - ComplexNumber") {
    val res = ComplexNumber(4, 3) - ComplexNumber(3, 1)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("1 + 2i"), res)
  }

  test("ComplexNumber - ComplexNumber = RealNumber") {
    val res = (ComplexNumber(4, 3) - ComplexNumber(-4, 3)).evaluate
    assert(res.isInstanceOf[RealNumber])
    assert(res.toString.equals("8"), res)
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
    assert(res.toString.equals("32 + 8i"), res)
  }

  test("-ComplexNumber * RealNumber") {
    val res = ComplexNumber(-8, 2) * RealNumber(4)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("-32 + 8i"), res)
  }

  test("ComplexNumber * Variable") {
    val res = ComplexNumber(4, 3) * Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 + 3i * X"), res)
  }

  test("ComplexNumber * Variable - 1") {
    val res = (ComplexNumber(4, 0) * Variable("X")).evaluate
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4 * X"), res)
  }

  test("ComplexNumber * Indeterminate") {
    val res = ComplexNumber(4, 3) * Indeterminate(4, "X", 3.5)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 + 3i * 4 * X^3.5"), res)
  }

  test("ComplexNumber * Indeterminate - 1") {
    val res = (ComplexNumber(4, 0) * Indeterminate(4, "X", 3.5)).evaluate
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("16 * X^3.5"), res)
  }

  test("ComplexNumber * ComplexNumber") {
    val res = (ComplexNumber(4, 2) * ComplexNumber(4, 3)).evaluate
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("10 + 20i"), res)
  }

  test("ComplexNumber * ComplexNumber - 1") {
    val res = (ComplexNumber(4, 2) * ComplexNumber(4, -3)).evaluate
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("22 - 4i"), res)
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
    assert(res.toString.equals("0.5 + 1i"), res)
  }

  test("ComplexNumber / Variable") {
    val res = ComplexNumber(4, 23) / Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 + 23i / X"), res)
  }

  test("ComplexNumber / Indeterminate") {
    val res = ComplexNumber(4, 45) / Indeterminate(4, "X", 2)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4 + 45i / 4 * X^2"), res)
  }

  test("ComplexNumber / Indeterminate - 1") {
    val res = (ComplexNumber(16, 0) / Indeterminate(4, "X", 2)).evaluate
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4 * X^-2"), res)
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
  test("ComplexNumber ^ Variable") {
    val res = ComplexNumber(2, 4) ^ Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 + 4i ^ X"))
  }

  test("ComplexNumber ^ Indeterminate") {
    val res = ComplexNumber(2, 4) ^ Indeterminate(4, "X", 4)
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("2 + 4i ^ 4 * X^4"))
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
