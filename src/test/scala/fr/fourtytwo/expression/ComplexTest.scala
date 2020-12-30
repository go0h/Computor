package fr.fourtytwo.expression

import fr.fourtytwo.computor.{Computor, TOKENIZER}
import org.scalatest.funsuite.AnyFunSuite


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

  test("ComplexNumber / RealNumber") {
    val res = ComplexNumber(2, 4) / RealNumber(4)
    assert(res.isInstanceOf[ComplexNumber])
    assert(res.toString.equals("0.5 + 1.0i"), res)
  }

  test("ComplexNumber + Variable") {
    val res = ComplexNumber(4, 3) + Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 3.0i + X"), res)
  }

  test("ComplexNumber + Variable - 1") {
    val res = (ComplexNumber(3, 0) + Variable("X")).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("3.0 + X"), res)
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

  test("ComplexNumber / Variable") {
    val res = ComplexNumber(4, 23) / Variable("X")
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 23.0i / X"), res)
  }

  test("ComplexNumber + Indeterminate") {
    val res = ComplexNumber(4, 2) + Indeterminate(RealNumber(2), Variable("X"), RealNumber(3))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 2.0i + 2.0 * X^3.0"), res)
  }

  test("ComplexNumber + Indeterminate = 1") {
    val res = (ComplexNumber(4, 0) + Indeterminate(RealNumber(2), Variable("X"), RealNumber(3))).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 2.0 * X^3.0"), res)
  }

  test("ComplexNumber - Indeterminate") {
    val res = ComplexNumber(4, 2) - Indeterminate(RealNumber(2), Variable("X"), RealNumber(3))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 2.0i - 2.0 * X^3.0"), res)
  }

  test("ComplexNumber - Indeterminate - 1") {
    val res = (ComplexNumber(4, 0) - Indeterminate(RealNumber(2), Variable("X"), RealNumber(3))).evaluate
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 - 2.0 * X^3.0"), res)
  }

  test("ComplexNumber * Indeterminate") {
    val res = ComplexNumber(4, 3) * Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(3.5))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 3.0i * 4.0 * X^3.5"), res)
  }

  test("ComplexNumber * Indeterminate - 1") {
    val res = (ComplexNumber(4, 0) * Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(3.5))).evaluate
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("16.0 * X^3.5"), res)
  }

  test("ComplexNumber / Indeterminate") {
    val res = ComplexNumber(4, 45) / Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(2.0))
    assert(res.isInstanceOf[Operator])
    assert(res.toString.equals("4.0 + 45.0i / 4.0 * X^2.0"), res)
  }

  test("ComplexNumber / Indeterminate - 1") {
    val res = (ComplexNumber(16, 0) / Indeterminate(RealNumber(4.0), Variable("X"), RealNumber(2.0))).evaluate
    assert(res.isInstanceOf[Indeterminate])
    assert(res.toString.equals("4.0 * X^-2.0"), res)
  }

}
