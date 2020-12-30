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
  def +(other: RealNumber): Expression = Operator(this, "+", other)
  def +(other: Variable): Expression = Operator(this, "+", other)
  def +(other: Indeterminate): Expression = Operator(this, "+", other)
  def +(other: ComplexNumber): Expression = Operator(this, "+", other)
  def +(other: Matrix): Expression = Operator(this, "+", other)


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
  def -(other: RealNumber): Expression = Operator(this, "-", other)
  def -(other: Variable): Expression = Operator(this, "-", other)
  def -(other: Indeterminate): Expression = Operator(this, "-", other)
  def -(other: ComplexNumber): Expression = Operator(this, "-", other)
  def -(other: Matrix): Expression = Operator(this, "-", other)


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
  def *(other: RealNumber): Expression = Operator(this, "*", other)
  def *(other: Variable): Expression = Operator(this, "*", other)
  def *(other: Indeterminate): Expression = Operator(this, "*", other)
  def *(other: ComplexNumber): Expression = Operator(this, "*", other)
  def *(other: Matrix): Expression = Operator(this, "*", other)


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
  def /(other: RealNumber): Expression = Operator(this, "/", other)
  def /(other: Variable): Expression = Operator(this, "/", other)
  def /(other: Indeterminate): Expression = Operator(this, "/", other)
  def /(other: ComplexNumber): Expression = Operator(this, "/", other)
  def /(other: Matrix): Expression = Operator(this, "/", other)

  ////////////////////////////////////////
  ///////////// POWER METHODS ////////////
  ////////////////////////////////////////
  def ^(other: Expression): Expression = {
    other match {
      case rn : RealNumber => this ^ rn
      case _ => Operator(this, "^", other)
    }
  }
  def ^(other: RealNumber): Expression = throwException("power", other.getType)

  def %(other: Expression): Expression = {
    other match {
      case rn : RealNumber => {
        this match {
          case number: RealNumber => RealNumber(number.getNum % rn.getNum)
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
