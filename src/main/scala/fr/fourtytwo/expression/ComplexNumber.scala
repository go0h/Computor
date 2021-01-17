package fr.fourtytwo.expression

import fr.fourtytwo.math._

class ComplexNumber(re: Double, im: Double) extends Operable {

  def evaluate: Expression = {
    if (abs(im) > 0.000001) {
      if (abs(re) > 0.000001) this
      else ComplexNumber(0, im)
    } else {
      RealNumber(re)
    }
  }

  def changeSign: Expression = ComplexNumber(-re, -im)

  def simplify: Expression = if (im == 0) RealNumber(re) else this

  def getRe: Double = re
  def getIm: Double = im

  ////////////////////////////////////////
  /////////// ADDITION METHODS ///////////
  ////////////////////////////////////////
  override def +(other: RealNumber): Expression = ComplexNumber(other.getNum + re, im)
  override def +(other: Variable): Expression = Operator(this, "+", other)
  override def +(other: Indeterminate): Expression = Operator(this, "+", other)
  override def +(other: ComplexNumber): Expression = {
    ComplexNumber(re + other.getRe, im + other.getIm).simplify
  }
  override def +(other: Matrix): Expression = throwException("+", other)

  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  override def -(other: RealNumber): Expression = ComplexNumber(re - other.getNum, im)
  override def -(other: Variable): Expression = Operator(this, "-", other)
  override def -(other: Indeterminate): Expression = Operator(this, "-", other)
  override def -(other: ComplexNumber): Expression = {
    ComplexNumber(re - other.getRe, im - other.getIm).simplify
  }
  override def -(other: Matrix): Expression = throwException("-", other)


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  override def *(other: RealNumber): Expression = {
    ComplexNumber(other.getNum * re, other.getNum * im).simplify
  }
  override def *(other: Variable): Expression = Operator(this, "*", other)
  override def *(other: Indeterminate): Expression = Operator(this, "*", other)
  override def *(other: ComplexNumber): Expression = {
    if (re == other.getRe && im == -other.getIm)
      return RealNumber(re * other.getRe + im * (-other.getIm))
    val newRe = re * other.getRe - im * other.getIm
    val newIm = im * other.getRe + re * other.getIm
    ComplexNumber(newRe, newIm).simplify
  }
  override def *(other: Matrix): Expression = throwException("*", other)


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  override def /(other: RealNumber): Expression = {
    if (other == 0)
      throw new ArithmeticException("Division by zero")
    this / ComplexNumber(other.getNum, 0)
  }
  override def /(other: Variable): Expression = Operator(this, "/", other)
  override def /(other: Indeterminate): Expression = Operator(this, "/", other)
  override def /(other: ComplexNumber): Expression = {
    val newRe = ((re * other.getRe) + (im * other.getIm)) / (other.getRe * other.getRe + other.getIm * other.getIm)
    val newIm = ((im * other.getRe) - (re * other.getIm)) / (other.getRe * other.getRe + other.getIm * other.getIm)
    ComplexNumber(newRe, newIm).simplify
  }

  ////////////////////////////////////////
  ////////////// POWER METHODS ///////////
  ////////////////////////////////////////
  override def ^(other: RealNumber): Expression = {

    val n = other.getNum
    val fi = math.atan(im / re)
    val zPow = pow(sqrt(re * re + im * im), n)

    ComplexNumber(zPow * math.cos(n * fi), zPow * math.sin(n * fi)).evaluate
  }


  def compare(other: Operable): Int = {
    other match {
      case _ : RealNumber => -1
      case _ : ComplexNumber => 0
      case _ : Variable => 1
      case _ : Indeterminate => 1
      case _ : Matrix => 1
    }
  }

  def contains(op: String): Boolean = false

  override def toString: String = {
    s"${if (re.round == re) re.round else re} ${if (im < 0) "-" else "+"} ${if (im.round == im) abs(im).round else abs(im)}i"
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case c: ComplexNumber => re == c.getRe && im == c.getIm
      case _ => obj.equals(this)
    }
  }
}

object ComplexNumber {
  def apply(re: Double, im: Double): ComplexNumber = new ComplexNumber(re, im)
}
