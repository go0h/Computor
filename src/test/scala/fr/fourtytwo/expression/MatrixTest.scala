package fr.fourtytwo.expression

import fr.fourtytwo.exception.{EvaluateException, ParseException}
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

  test("Matrix + Matrix - 1") {
    val expr1 = "[ [1, 7]; [0, 2]]"
    val expr2 = "[ [1, 0]; [2, 1.5]]"
    val res =   "[ [2, 7]; [2, 3.5]]"

    assert((Matrix(expr1) + Matrix(expr2)).equals(Matrix(res)))
  }

  test("Matrix + diff size Matrix") {
    val expr1 = "[ [1, 7]; [0, 2]]"
    val expr2 = "[ [1, 0]; [2, 1.5]; [4, 0.25]]"

    assertThrows[EvaluateException]{
      Matrix(expr1) + Matrix(expr2)
    }
  }

  test("Matrix - Matrix - 1") {
    val expr1 = "[ [1, 7]; [0, 2]]"
    val expr2 = "[ [1, 0]; [2, 1.5]]"
    val res =   "[ [0, 7]; [-2, 0.5]]"

    assert((Matrix(expr1) - Matrix(expr2)).equals(Matrix(res)))
  }

  test("Matrix - diff size Matrix") {
    val expr1 = "[ [1, 7]; [0, 2]]"
    val expr2 = "[ [1, 0]; [2, 1.5]; [4, 0.25]]"

    assertThrows[EvaluateException]{
      Matrix(expr1) - Matrix(expr2)
    }
  }

  test("Matrix * RealNumber") {

    val expr1 = "[ [1, 7]; [0, 2.5]]"
    val expr2 = "[ [2, 14]; [0, 5]]"

    assert((Matrix(expr1) * RealNumber(2)).equals(Matrix(expr2)))
  }

  test("Matrix * Matrix - 1") {

    val expr1 = "[ [1,2,3];[4,5,6] ]"
    val expr2 = "[ [1,2,3,4]; [1,2,3,4]; [1,2,3,4] ]"
    val res = """[ [6.0 , 12.0, 18.0, 24.0];
                |  [15.0, 30.0, 45.0, 60.0] ]""".stripMargin

    assert((Matrix(expr1) * Matrix(expr2)).equals(Matrix(res)))
  }

  test("Matrix * Matrix - 2") {

    val expr1 = "[ [1, 2, 3]; [4, 5, 6] ]"
    val expr2 = "[ [2.5]; [2.5]; [5] ]"
    val res =   "[ [22.5]; [52.5] ]"

    assert((Matrix(expr1) * Matrix(expr2)).equals(Matrix(res)))
  }

  test("Matrix * Matrix - 3") {

    val expr1 = "[ [1]; [2]; [3]; [4]; [5]]"
    val expr2 = "[ [1, 2,  3,  4,  5] ]"
    val res = """[ [1, 2,  3,  4,  5];
                | [2, 4,  6,  8,  10];
                | [3, 6,  9,  12, 15];
                | [4, 8,  12, 16, 20];
                | [5, 10, 15, 20, 25]]""".stripMargin

    assert((Matrix(expr1) * Matrix(expr2)).equals(Matrix(res)))
  }

  test("Matrix * Matrix - 4") {

    val expr1 = "[ [2] ]"
    val expr2 = "[ [1, 2,  3,  4,  5] ]"
    val res =   "[ [2, 4,  6,  8,  10] ]"

    assert((Matrix(expr1) * Matrix(expr2)).equals(Matrix(res)))
  }

  test("Matrix * Matrix - 5") {

    val expr1 = "[ [1, 2, 3]; [4, 5, 6] ]"
    val expr2 = "[ [2.5]; [2.5]; [5] ]"
    val res =   "[ [22.5]; [52.5] ]"

    assert((Matrix(expr1) * Matrix(expr2)).equals(Matrix(res)))
  }

  test("Matrix * Matrix - 6") {

    val expr1 = "[ [1,2,3]; [1,2,3] ]"
    val expr2 = "[ [3,2,1]; [3,2,1]; [4,5,5] ]"
    val res =   "[ [21, 21, 18]; [21, 21, 18] ]"

    assert((Matrix(expr1) * Matrix(expr2)).equals(Matrix(res)))
  }
}
