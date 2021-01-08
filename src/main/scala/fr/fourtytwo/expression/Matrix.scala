package fr.fourtytwo.expression

import scala.collection.mutable.ArrayBuffer
import fr.fourtytwo.exception.{EvaluateException, ParseException}
import fr.fourtytwo.expression.Matrix.validateMatrix


class Matrix(matrix: Array[Array[RealNumber]]) extends Operable {

  def this(expr: String) = {
    this(validateMatrix(expr))
  }

  ////////////////////////////////////////
  /////////// ADDITION METHODS ///////////
  ////////////////////////////////////////
  override def +(other: RealNumber): Expression = throwException("+", other)
  override def +(other: Variable): Expression = Operator(this, "+", other)
  override def +(other: Indeterminate): Expression = throwException("+", other)
  override def +(other: ComplexNumber): Expression = throwException("+", other)

  override def +(other: Matrix): Expression = {

    if (getWidth != other.getWidth || getHeight != other.getHeight)
      throw new EvaluateException(s"Can't add different matrices: $toString\n ${other.toString}")

    val sum = for (i <- 0 until getHeight) yield {
      for (j <- 0 until getWidth) yield {
        matrix(i)(j) + other.getMatrix(i)(j)
      }
    }.toArray

    Matrix(sum.toArray)
  }

  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  override def -(other: RealNumber): Expression = throwException("-", other)
  override def -(other: Variable): Expression = Operator(this, "-", other)
  override def -(other: Indeterminate): Expression = throwException("-", other)
  override def -(other: ComplexNumber): Expression = throwException("-", other)

  override def -(other: Matrix): Matrix = {

    if (getWidth != other.getWidth || getHeight != other.getHeight)
      throw new EvaluateException(s"Can't add different matrices: $toString\n ${other.toString}")

    val sub = for (i <- 0 until getHeight) yield {
      for (j <- 0 until getWidth) yield {
        matrix(i)(j) - other.getMatrix(i)(j)
      }
    }.toArray

    Matrix(sub.toArray)
  }


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  override def *(other: RealNumber): Expression = {

    val sum = for (i <- 0 until getHeight) yield {
      for (j <- 0 until getWidth) yield {
        matrix(i)(j) * other
      }
    }.toArray

    Matrix(sum.toArray)
  }
  override def *(other: Indeterminate): Expression = throwException("*", other)
  override def *(other: ComplexNumber): Expression = throwException("*", other)


  /** Term-by-term multiplication */
  override def *(other: Matrix): Expression = {

    if (getWidth != other.getWidth || getHeight != other.getHeight) {
      throw new EvaluateException("Can't multiply term-by-term matrices with different sizes")
    }

    val otherMatrix = other.getMatrix

    val sum = for (i <- 0 until getHeight) yield {
      for (j <- 0 until getWidth) yield {
        matrix(i)(j) * otherMatrix(i)(j)
      }
    }.toArray

    Matrix(sum.toArray)
  }


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  override def /(other: RealNumber): Expression = throwException("/", other)
  override def /(other: Variable): Expression = throwException("/", other)
  override def /(other: Indeterminate): Expression = throwException("/", other)
  override def /(other: ComplexNumber): Expression = throwException("/", other)
  override def /(other: Matrix): Expression = throwException("/", other)


  /** True matrix multiplication */
  def **(other: Matrix): Expression = {

    if (getWidth != other.getHeight) {
      throw new EvaluateException("Can't multiply matrices with different sizes")
    }

    val otherMatrix = other.getMatrix

    val row = (for (_ <- 0 until other.getWidth) yield RealNumber(0)).toArray
    val newMatrix = (for (_ <- 0 until getHeight) yield row.clone()).toArray

      for (i <- 0 until getHeight) {
        for (j <- 0 until other.getWidth) {

          var res = 0.0
          for (k <- 0 until getWidth) {
            res += matrix(i)(k).getNum * otherMatrix(k)(j).getNum
          }

          newMatrix(i)(j) = RealNumber(res)
        }
      }

    Matrix(newMatrix)
  }

  def getWidth: Int = matrix(0).length
  def getHeight: Int = matrix.length
  def getMatrix: Array[Array[RealNumber]] = matrix

  def compare(other: Operable): Int = {
    other match {
      case _ : RealNumber => -1
      case _ : ComplexNumber => -1
      case _ : Variable => 1
      case _ : Indeterminate => 1
      case m : Matrix => {
        for (i <- 0 until getHeight) {
          for (j <- 0 until getWidth) {
            val comp = matrix(i)(j).compare(m.getMatrix(i)(j))
            if (comp != 0)
              return comp
          }
        }
        0
      }
    }
  }

  def evaluate: Expression = this

  def changeSign: Expression = {
    val res = for (i <- 0 until getHeight) yield {
      for (j <- 0 until getWidth) yield {
        matrix(i)(j).changeSign
      }
    }.toArray
    Matrix(res.toArray)
  }

  def contains(op: String): Boolean = false

  override def equals(obj: Any): Boolean = {
    obj match {
      case m: Matrix => {

        if (getHeight != m.getHeight || getWidth != m.getWidth)
          return false

        for (i <- 0 until getHeight) {
          for (j <- 0 until getWidth) {
            if (!matrix(i)(j).equals(m.getMatrix(i)(j)))
              return false
          }
        }
        true
      }
      case _ => false
    }
  }

  override def toString: String = {

    val str = matrix.map(_.map(_.toString))

    val maxColLength =
      for (i <- matrix(0).indices) yield {
        for (j <- matrix.indices) yield {
          matrix(j)(i).toString.length
        }
      }.toArray.max

    val aligned = str.map(x => {
      for (i <- x.indices) yield x(i) + (" " * (maxColLength(i) - x(i).length))
    })

    aligned.map(_.mkString("[ ", ", ", " ]")).mkString("\n")
  }

}

object Matrix {

  def apply(expr: String): Matrix = new Matrix(expr)

  def apply(matrix: Array[Array[RealNumber]]): Matrix = new Matrix(matrix)

  def validateMatrix(expression: String): Array[Array[RealNumber]] = {

    val expr = expression.replaceAll("\\s+", "")

    if (expr == null || expr.isEmpty)
      throw new ParseException("Matrix is empty")

    if (!expr.head.equals('[') || !expr.last.equals(']'))
      throw new ParseException(s"Matrix $expr not well formatted")

    val rows = expr.slice(1, expr.length - 1).split(";")

    if (rows.isEmpty)
      throw new ParseException(s"Matrix $expr is empty")

    val matrix = new ArrayBuffer[Array[RealNumber]]()

    var c = 0
    for (row <- rows) {

      if (row.isEmpty || !row.head.equals('[') || !row.last.equals(']'))
        throw new ParseException(s"Matrix $expr not well formatted")

      val cols = row.slice(1, row.length - 1).split(",")

      if ((c != 0 && cols.length != c) || (c == 0 && cols.isEmpty))
        throw new ParseException(s"Matrix $expr not well formatted")
      c = if (c == 0) cols.length else c

      val rowD = for (cell <- cols) yield {
        try {
         RealNumber(cell.toDouble)
        } catch {
          case _: NumberFormatException => throw new ParseException(s"Matrix $expr not well formatted")
        }
      }

      matrix.append(rowD)
    }

    matrix.toArray
  }

}