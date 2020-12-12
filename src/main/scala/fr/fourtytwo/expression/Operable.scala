package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException

//TODO maybe add modulo division
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
    }
  }
  def +(other: RealNumber): Expression
  def +(other: Variable): Expression
  def +(other: Indeterminate): Expression
  def +(other: ComplexNumber): Expression


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  def -(other: Operable): Expression = {
    other match {
      case rn : RealNumber => this - rn
      case v : Variable => this - v
      case i : Indeterminate => this - i
      case cn : ComplexNumber => this - cn
    }
  }
  def -(other: RealNumber): Expression
  def -(other: Variable): Expression
  def -(other: Indeterminate): Expression
  def -(other: ComplexNumber): Expression


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  def *(other: Expression): Expression = {
    other match {
      case rn : RealNumber => this * rn
      case v : Variable => this * v
      case i : Indeterminate => this * i
      case cn : ComplexNumber => this * cn
    }
  }
  def *(other: RealNumber): Expression
  def *(other: Variable): Expression
  def *(other: Indeterminate): Expression
  def *(other: ComplexNumber): Expression


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  def /(other: Expression): Expression = {
    other match {
      case rn : RealNumber => this / rn
      case v : Variable => this / v
      case i : Indeterminate => this / i
      case cn : ComplexNumber => this / cn
    }
  }
  def /(other: RealNumber): Expression
  def /(other: Variable): Expression
  def /(other: Indeterminate): Expression
  def /(other: ComplexNumber): Expression

  ////////////////////////////////////////
  ///////////// POWER METHODS ////////////
  ////////////////////////////////////////
  def ^(other: Expression): Expression = {
    other match {
      case rn : RealNumber => this ^ rn
      case v : Variable => throw new EvaluateException(s"Can't raise '$toString' to the power '$v'")
      case i : Indeterminate => throw new EvaluateException(s"Can't raise '$toString' to the power '$i'")
      case cn : ComplexNumber => throw new EvaluateException(s"Can't raise '$toString' to the power '$cn'")
    }
  }
  def ^(other: RealNumber): Expression


  override def compare(other: Operable): Int
}
