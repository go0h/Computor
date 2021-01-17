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
  def /(other: Matrix): Expression = throwException("/", other)

  ////////////////////////////////////////
  ///////////// POWER METHODS ////////////
  ////////////////////////////////////////
  def ^(other: Expression): Expression = {

    if (other.isInstanceOf[ComplexNumber] || other.isInstanceOf[Matrix])
      throwException("^", other)

    if (isInstanceOf[Matrix])
      throwException("^", other)

    other match {
      case n : RealNumber => this ^ n
      case _ : Variable => Operator(this, "^", other)
      case _ : Indeterminate => Operator(this, "^", other)
    }
  }
  def ^(other: RealNumber): Expression = throwException("^", other)

  def %(other: Expression): Expression = {

    if (isInstanceOf[ComplexNumber] || isInstanceOf[Matrix])
      throwException("%", other)

    if (other.isInstanceOf[ComplexNumber] || other.isInstanceOf[Matrix])
      throwException("%", other)

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

  def **(other: Expression): Expression = {

    this match {
      case m1: Matrix =>
        other match {
          case m2: Matrix => m1 ** m2
          case _: Variable => Operator(this, "**", other)
        }
      case _: Variable =>
        other match {
          case _: Matrix => Operator(this, "**", other)
          case _: Variable => Operator(this, "**", other)
        }
      case _ => throwException("**", other)
    }
  }

  override def compare(other: Operable): Int

  def throwException(op: String, other: Expression): Expression = {
    throw new EvaluateException(s"Can't execute operator $op between $getType and ${other.getType}")
  }

}
