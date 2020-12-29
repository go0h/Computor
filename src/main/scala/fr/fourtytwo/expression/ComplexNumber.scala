package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException

class ComplexNumber(re: Double, im: Double) extends Operable {

  def evaluate: Double = throw new EvaluateException(s"Can't evaluate ComplexNumber $toString")
  def simplify: Expression = if (im != 0) this else RealNumber(re)
  def changeSign: Expression = ComplexNumber(-re, -im)

  def getRe: Double = re
  def getIm: Double = im

  ////////////////////////////////////////
  /////////// ADDITION METHODS ///////////
  ////////////////////////////////////////
  override def +(other: RealNumber): Expression = ComplexNumber(other.evaluate + re, im)
  override def +(other: Variable): Expression = Operator(this, "+", other)
  override def +(other: Indeterminate): Expression = Operator(this, "+", other)
  override def +(other: ComplexNumber): Expression = ComplexNumber(re + other.getRe, im + other.getIm)


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  override def -(other: RealNumber): Expression = ComplexNumber(re - other.evaluate, im)
  override def -(other: Variable): Expression = Operator(this, "-", other)
  override def -(other: Indeterminate): Expression = Operator(this, "-", other)
  override def -(other: ComplexNumber): Expression = {
    if (equals(other)) RealNumber(0)
    else ComplexNumber(re - other.getRe, im - other.getIm)
  }


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  override def *(other: RealNumber): Expression = {
    if (other.evaluate == 0) RealNumber(0)
    else ComplexNumber(other.evaluate * re, other.evaluate * im)
  }
  override def *(other: Variable): Expression = Operator(this, "*", other)
  override def *(other: Indeterminate): Expression = Operator(this, "*", other)
  override def *(other: ComplexNumber): Expression = {
    if (re == other.getRe && im == -other.getIm)
      return RealNumber(re * other.getRe + im * (-other.getIm))
    val newRe = re * other.getRe - im * other.getIm
    val newIm = im * other.getRe + re * other.getIm
    ComplexNumber(newRe, newIm)
  }


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  override def /(other: RealNumber): Expression = {
    if (other.evaluate == 0)
      throw new ArithmeticException("Division by zero")
    this / ComplexNumber(other.evaluate, 0)
  }
  override def /(other: Variable): Expression = Operator(this, "/", other)
  override def /(other: Indeterminate): Expression = Operator(this, "/", other)
  override def /(other: ComplexNumber): Expression = {
    val newRe = ((re * other.getRe) + (im * other.getIm)) / (other.getRe * other.getRe + other.getIm * other.getIm)
    val newIm = ((im * other.getRe) - (re * other.getIm)) / (other.getRe * other.getRe + other.getIm * other.getIm)
    ComplexNumber(newRe, newIm).simplify
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
    s"(${if (re == 0) 0.0 else re} ${if (im < 0) "-" else "+"} ${math.abs(im)}i)"
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
