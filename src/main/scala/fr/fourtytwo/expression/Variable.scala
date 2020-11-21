package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException

class Variable(name: String) extends Operable {

  val sign: Int = if (name.startsWith("-")) -1 else 1

  def evaluate: Double = throw new EvaluateException(s"Variable $name has no value")
  def optimize: Expression = this

  def getSing: Int = sign
  def changeSign: Variable = {
    if (name.startsWith("-"))
      return new Variable(name.replaceFirst("-", ""))
    Variable("-" + name)
  }

  ////////////////////////////////////////
  /////////// ADDITION METHODS ///////////
  ////////////////////////////////////////
  def +(other: RealNumber): Expression = {
    if (other.evaluate == 0.0)
      return this
    Operator(this, "+", other)
  }

  def +(other: Variable): Expression = {
    if (!equals(other))
      throw new EvaluateException(s"Can't add different variables ($name, $other)")
    Indeterminate(RealNumber(2.0), this, RealNumber(1.0))
  }

  def +(other: Indeterminate): Expression = {
    if (!equals(other.variable))
      throw new EvaluateException(s"Can't add different variables ($name, ${other.variable})")
    if (other.degree.evaluate == 1.0)
      return Indeterminate(RealNumber(other.constant.evaluate + 1.0), this, RealNumber(1.0))
    Operator(this, "+", other)
  }


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  def -(other: RealNumber): Expression = {
    if (other.evaluate == 0)
      return this
    Operator(this, "-", other)
  }

  def -(other: Variable): Expression = {
    if (!equals(other))
      throw new EvaluateException(s"Can't sub different variables ($name, $other)")
    RealNumber(0.0)
  }

  def -(other: Indeterminate): Expression = {
    if (!equals(other.variable))
      throw new EvaluateException(s"Can't sub different variables ($name, ${other.variable})")
    if (other.degree.evaluate == 1.0) {
      if (other.constant.evaluate == 1.0)
        return RealNumber(0.0)
      return new Indeterminate(1 - other.constant.evaluate, name, other.degree.evaluate)
    }
    Operator(this, "-", other)
  }


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  def *(other: RealNumber): Indeterminate = new Indeterminate(other.evaluate, name, 1.0)

  def *(other: Variable): Indeterminate = {
    if (!name.equals(other.toString))
      throw new EvaluateException(s"Can't multiply different variables ($name, ${other.toString})")
    new Indeterminate(1.0, name, 2.0)
  }

  def *(other: Indeterminate): Indeterminate = {
    if (!equals(other.variable))
      throw new EvaluateException(s"Can't multiply different variables ($name, ${other.variable})")
    Indeterminate(other.constant,
                  other.variable,
                  RealNumber(other.degree.evaluate + 1))
  }


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  def /(other: RealNumber): Indeterminate = {
    new Indeterminate(1 / other.evaluate, name, 1)
  }

  def /(other: Variable): Operable = {
    if (!name.equals(other.toString))
      throw new EvaluateException(s"Can't division different variables ($name, ${other.toString})")
    new RealNumber(1)
  }

  def /(other: Indeterminate): Operable = {
    if (!name.equals(other.variable.toString))
      throw new EvaluateException(s"Can't division different variables ($name, ${other.variable})")
    if (other.degree.evaluate == 1)
      return other.constant
    new Indeterminate(RealNumber(1 / other.constant.evaluate),
                      Variable(name),
                      RealNumber((other.degree.evaluate - 1) * -1))
  }

  override def equals(other: Any): Boolean = {
      other match {
        case _: Variable => name.equals(other.toString)
        case _: Throwable => false
    }
  }

  override def toString: String = name
}

object Variable {
  def apply(name: String): Variable = new Variable(name)
}