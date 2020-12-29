package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException


trait Operable extends Expression with Ordered[Operable] {

  ////////////////////////////////////////
  //////////// ADDITION METHODS //////////
  ////////////////////////////////////////
    def +(other: Operable): Expression = {
      other match {
        case rn : RealNumber => this + rn
        case v : Variable => this + v
        case i : Indeterminate => this + i
        case cn : ComplexNumber => this + cn
        case m : Matrix => this + m
      }
    }
  def +(other: RealNumber): Expression = throwException("add", other.getType)
  def +(other: Variable): Expression = throwException("add", other.getType)
  def +(other: Indeterminate): Expression = throwException("add", other.getType)
  def +(other: ComplexNumber): Expression = throwException("add", other.getType)
  def +(other: Matrix): Expression = throwException("add", other.getType)


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  def -(other: Operable): Expression = {
    other match {
      case rn : RealNumber => this - rn
      case v : Variable => this - v
      case i : Indeterminate => this - i
      case cn : ComplexNumber => this - cn
      case m : Matrix => this - m
    }
  }
  def -(other: RealNumber): Expression = throwException("sub", other.getType)
  def -(other: Variable): Expression = throwException("sub", other.getType)
  def -(other: Indeterminate): Expression = throwException("sub", other.getType)
  def -(other: ComplexNumber): Expression = throwException("sub", other.getType)
  def -(other: Matrix): Expression = throwException("sub", other.getType)


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  def *(other: Expression): Expression = {
    other match {
      case rn : RealNumber => this * rn
      case v : Variable => this * v
      case i : Indeterminate => this * i
      case cn : ComplexNumber => this * cn
      case m : Matrix => this * m
    }
  }
  def *(other: RealNumber): Expression = throwException("mul", other.getType)
  def *(other: Variable): Expression = throwException("mul", other.getType)
  def *(other: Indeterminate): Expression = throwException("mul", other.getType)
  def *(other: ComplexNumber): Expression = throwException("mul", other.getType)
  def *(other: Matrix): Expression = throwException("mul", other.getType)


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  def /(other: Expression): Expression = {
    other match {
      case rn : RealNumber => this / rn
      case v : Variable => this / v
      case i : Indeterminate => this / i
      case cn : ComplexNumber => this / cn
      case m : Matrix => this / m
    }
  }
  def /(other: RealNumber): Expression = throwException("div", other.getType)
  def /(other: Variable): Expression = throwException("div", other.getType)
  def /(other: Indeterminate): Expression = throwException("div", other.getType)
  def /(other: ComplexNumber): Expression = throwException("div", other.getType)
  def /(other: Matrix): Expression = throwException("div", other.getType)

  ////////////////////////////////////////
  ///////////// POWER METHODS ////////////
  ////////////////////////////////////////
  def ^(other: Expression): Expression = {
    other match {
      case rn : RealNumber => this ^ rn
      case _  : Matrix => throwException("power", other.getType)
      case _ => Operator(this, "%", other)
    }
  }
  def ^(other: RealNumber): Expression = throwException("power", other.getType)

  def %(other: Expression): Expression = {
    other match {
      case rn : RealNumber => {
        this match {
          case number: RealNumber => RealNumber(number.evaluate % rn.evaluate)
          case _ => Operator(this, "%", other)
        }
      }
      case _ => {
        if (equals(other)) RealNumber(0)
        else Operator(this, "%", other)
      }
    }
  }

  override def compare(other: Operable): Int

  def throwException(op: String, other: String): Expression = {
    throw new EvaluateException(s"Can't $op $other to $getType")
  }
}
