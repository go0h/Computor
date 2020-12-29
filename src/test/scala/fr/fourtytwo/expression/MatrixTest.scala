package fr.fourtytwo.expression

import fr.fourtytwo.exception.ParseException
import org.scalatest.funsuite.AnyFunSuite

class MatrixTest extends AnyFunSuite {

  test("Empty - 1") {
    assertThrows[NullPointerException] {
      Matrix.validateMatrix(null)
    }
    assertThrows[ParseException] {
      Matrix.validateMatrix("")
    }
  }

  test("Empty - 2") {
    assertThrows[ParseException] {
      Matrix.validateMatrix("[[];[]]")
    }
    assertThrows[ParseException] {
      Matrix.validateMatrix("[ ]")
    }
  }

  test("No brackets - 1") {
    val matrix = "[[1,0];[0,1]"
    assertThrows[ParseException] {
      Matrix.validateMatrix(matrix)
    }
  }

  test("No brackets - 2") {
    val matrix = "[[1,0];0,1]]"
    assertThrows[ParseException] {
      Matrix.validateMatrix(matrix)
    }
  }

  test("No brackets - 3") {
    val matrix = "[[1,0;[0,1]]"
    assertThrows[ParseException] {
      Matrix.validateMatrix(matrix)
    }
  }

  test("Without semicolon - 1") {
    val matrix = "[[1,0][0,1]]"
    assertThrows[ParseException] {
      Matrix.validateMatrix(matrix)
    }
  }

  test("Without comma - 1") {
    val matrix = "[[1,0];[0 1]]"
    assertThrows[ParseException] {
      Matrix.validateMatrix(matrix)
    }
  }

  test("Equal test - 1") {
    val expr1 = "[[1,0];[0,1]]"
    val expr2 = "[[1.0, 0];[0, 1.00]]"

    val m1 = Matrix(expr1)
    val m2 = Matrix(expr2)
    assert(m1.equals(m2))
  }

  test("Non equal test - 1") {
    val expr1 = "[[1,0];[0,2]]"
    val expr2 = "[[1.0, 0];[0, 1.00]]"

    val m1 = Matrix(expr1)
    val m2 = Matrix(expr2)
    assert(!m1.equals(m2))
  }

  test("Non equal test - 3") {

    val expr1 = "[[1,0];[0,2]]"
    val expr2 = "[[1,0,0];[0,2,1]]"

    val m1 = Matrix(expr1)
    val m2 = Matrix(expr2)
    assert(!m1.equals(m2))
  }

  test("Non equal test - 4") {

    val expr1 = "[[1,0];[0,2]]"
    val expr2 = "[[1,0,0];[0,2,1]]"

    val m1 = Matrix(expr1)
    val m2 = Matrix(expr2)
    assert(!m1.equals(m2))
  }

  test("Non equal test - 5") {
    val expr1 = "[[1,0];[0,1]]"
    val m1 = Matrix(expr1)
    assert(!m1.equals(RealNumber(0)))
  }

  test("Change sign - 1") {

    val expr1 = "[[11,0];[-3,2]]"
    val expr2 = "[[-11,-0];[3,-2]]"

    val m1 = Matrix(expr1).changeSign
    val m2 = Matrix(expr2)
    assert(m1.equals(m2))
  }

}
