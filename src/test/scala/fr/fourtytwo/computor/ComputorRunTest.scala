package fr.fourtytwo.computor

import java.io.{BufferedReader, ByteArrayInputStream, ByteArrayOutputStream, PrintStream, StringReader}
import org.scalatest.funsuite.AnyFunSuite


class ComputorRunTest extends AnyFunSuite {


  def createComputer(expr: String, stream: ByteArrayOutputStream): Unit = {

    val computor = new Computor()

    val reader = new BufferedReader(new StringReader(expr))
    val in = new BufferedReader(reader)
    val out = new PrintStream(stream, true, "UTF-8")
    computor.run(in, out)
  }

  test("Mandatory - RationalNumber") {

    val expr1: String =
      s"""varA = 2
         |varB = 4.242
         |varC = -4.3
         |""".stripMargin

    val res1: String =
      s"""2
         |4.242
         |-4.3
         |""".stripMargin

    val stream = new ByteArrayOutputStream()
    createComputer(expr1, stream)
    assert(stream.toString.equals(res1))

    stream.close()
  }

  test("Mandatory - ComplexNumber") {

    val expr2: String =
      s"""varA = 2*i + 3
         |varB = -4i - 4
         |""".stripMargin

    val res2: String =
      s"""3 + 2i
         |-4 - 4i
         |""".stripMargin

    val stream = new ByteArrayOutputStream()
    createComputer(expr2, stream)
    assert(stream.toString.equals(res2))

    stream.close()
  }

  test("Mandatory - Matrices") {

    val expr3: String =
      s"""varA = [[2,3];[4,3]]
         |varB = [[3,4]]
         |""".stripMargin

    val res3: String =
      s"""[ 2, 3 ]
         |[ 4, 3 ]
         |[ 3, 4 ]
         |""".stripMargin

    val stream = new ByteArrayOutputStream()
    createComputer(expr3, stream)
    assert(stream.toString.equals(res3))

    stream.close()
  }

  test("Mandatory - Functions") {

    val expr4: String =
      s"""funA(x) = 2*x^5 + 4x^2 - 5*x + 4
         |funB(y) = 43 * y / (4 % 2 * y)
         |funC(z) = -2 * z - 5
         |""".stripMargin

    val res4: String =
      s"""2 * x^5 + 4 * x^2 + -5 * x + 4
         |43 * y / 4 % 2 * y
         |-2 * z - 5
         |""".stripMargin

    val stream = new ByteArrayOutputStream()
    createComputer(expr4, stream)
    assert(stream.toString.equals(res4), stream.toString)

    stream.close()
  }

  test("Mandatory - Reassign a variable and change the type") {

    val expr5: String =
      s"""x = 2
         |y = x
         |y = 7
         |y = 2 * i - 4
         |""".stripMargin

    val res5: String =
      s"""2
         |2
         |7
         |-4 + 2i
         |""".stripMargin

    val stream = new ByteArrayOutputStream()
    createComputer(expr5, stream)
    assert(stream.toString.equals(res5), stream.toString)

    stream.close()
  }

  test("Mandatory - Reassign the result of a computation to a variable") {

    val expr6: String =
      s"""varA = 2 + 4 *2 - 5 %4 + 2 * (4 + 5)
         |varB = 2 * varA - 5 %4
         |funA(x) = varA + varB * 4 - 1 / 2 + x
         |varC = 2 * varA - varB
         |varD = funA(varC)
         |""".stripMargin

    val res6: String =
      s"""27
         |53
         |27 + 212 + -0.5 + x
         |1
         |239.5
         |""".stripMargin

    val stream = new ByteArrayOutputStream()
    createComputer(expr6, stream)
    assert(stream.toString.equals(res6), stream.toString)

    stream.close()
  }

  test("Mandatory - Resolution of a computation") {

    val expr7: String =
      s"""a = 2 * 4 + 4
         |a + 2 = ?
         |""".stripMargin

    val res7: String =
      s"""12
         |14
         |""".stripMargin

    val stream = new ByteArrayOutputStream()
    createComputer(expr7, stream)
    assert(stream.toString.equals(res7), stream.toString)

    stream.close()
  }

  test("Mandatory - Image computation") {

    val expr8: String =
      s"""funA(x) = 2 * 4 + x
         |funB(x) = 4 -5 + (x + 2)^2 - 4
         |funC(x) = 4x + 5 - 2
         |funA(2) + funB(4) = ?
         |funC(3) = ?
         |""".stripMargin

    val res8: String =
      s"""8 + x
         |-1 + (x + 2) ^ 2 + -4
         |4 * x + 5 + -2
         |41
         |15
         |""".stripMargin

    val stream = new ByteArrayOutputStream()
    createComputer(expr8, stream)
    assert(stream.toString.equals(res8), stream.toString)

    stream.close()
  }

  test("Mandatory - Rational or imaginary") {

    val expr9: String =
      s"""varA = 2
         |varB= 2 * (4 + varA + 3)
         |varC =2 * varB
         |varD  =     2 *(2 + 4 *varC -4 /3)
         |""".stripMargin

    val res8: String =
      s"""2
         |18
         |36
         |289.3333333333333
         |""".stripMargin

    val stream = new ByteArrayOutputStream()
    createComputer(expr9, stream)
    assert(stream.toString.equals(res8), stream.toString)

    stream.close()
  }

}
