package fr.fourtytwo.expression

import fr.fourtytwo.expression.Operator._

trait Operable extends Expression {

  ////////////////////////////////////////
  //////////// ADDITION METHODS //////////
  ////////////////////////////////////////
  def +(other: Operable): Expression = {
    other match {
      case rn : RealNumber => this + rn
      case v : Variable => this + v
      case i : Indeterminate => this + i
    }
  }
  def +(other: RealNumber): Expression
  def +(other: Variable): Expression
  def +(other: Indeterminate): Expression


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  def -(other: Operable): Expression = {
    other match {
      case rn : RealNumber => this - rn
      case v : Variable => this - v
      case i : Indeterminate => this - i
    }
  }
  def -(other: RealNumber): Expression
  def -(other: Variable): Expression
  def -(other: Indeterminate): Expression


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  def *(other: Expression): Expression = {
    other match {
      case rn : RealNumber => this * rn
      case v : Variable => this * v
      case i : Indeterminate => this * i
    }
  }
  def *(other: RealNumber): Expression
  def *(other: Variable): Expression
  def *(other: Indeterminate): Expression


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  def /(other: Expression): Expression = {
    other match {
      case rn : RealNumber => this / rn
      case v : Variable => this / v
      case i : Indeterminate => this / i
    }
  }
  def /(other: RealNumber): Expression
  def /(other: Variable): Expression
  def /(other: Indeterminate): Expression

}
