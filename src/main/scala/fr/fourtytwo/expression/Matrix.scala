package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException

class Matrix extends Operable {

  def +(other: RealNumber): Expression = ???
  def +(other: Variable): Expression = ???
  def +(other: Indeterminate): Expression = ???
  def +(other: ComplexNumber): Expression = ???

  def -(other: RealNumber): Expression = ???
  def -(other: Variable): Expression = ???
  def -(other: Indeterminate): Expression = ???
  def -(other: ComplexNumber): Expression = ???

  def *(other: RealNumber): Expression = ???
  def *(other: Variable): Expression = ???
  def *(other: Indeterminate): Expression = ???
  def *(other: ComplexNumber): Expression = ???

  def /(other: RealNumber): Expression = ???
  def /(other: Variable): Expression = ???
  def /(other: Indeterminate): Expression = ???
  def /(other: ComplexNumber): Expression = ???

  def ^(other: RealNumber): Expression = ???


  def compare(other: Operable): Int = ???

  def evaluate: Double = throw new EvaluateException(s"Can't evaluate matrix")

  def simplify: Expression = ???

  def changeSign: Expression = ???

  def contains(op: String): Boolean = false
}
