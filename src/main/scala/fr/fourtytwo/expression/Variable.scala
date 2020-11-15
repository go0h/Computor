package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException

class Variable(name: String, value: RealNumber = null) extends Expression {

  override def evaluate: Double = {
    if (value == null)
      throw new EvaluateException(s"Variable $value has no value")
    value.evaluate
  }

  def *(other: RealNumber): Indeterminate = new Indeterminate(other.evaluate, name, 1.0)

  def *(other: Variable): Indeterminate = {
    if (!name.equals(other.toString))
      throw new EvaluateException(s"Can't multiply different variables ($name, ${other.toString})")
    new Indeterminate(1.0, name, 2.0)
  }

  def *(other: Indeterminate): Indeterminate = {
    if (!equals(other.variable))
      throw new EvaluateException(s"Can't multiply different variables ($name, ${other.variable})")
    new Indeterminate(other.constant,
                      other.variable,
                      RealNumber(other.degree.evaluate * 2.0))
  }

  def /(other: RealNumber): Indeterminate = new Indeterminate(1.0 / other.evaluate, name, 1.0)

  def /(other: Variable): Expression = {
    if (!name.equals(other.toString))
      throw new EvaluateException(s"Can't division different variables ($name, ${other.toString})")
    new RealNumber(1.0)
  }

  def /(other: Indeterminate): Expression = {
    if (!name.equals(other.variable.toString))
      throw new EvaluateException(s"Can't division different variables ($name, ${other.variable})")
    if (other.degree.evaluate == 1.0)
      return other.constant
    new Indeterminate(RealNumber(1.0 / other.constant.evaluate),
                      Variable(name),
                      RealNumber((other.degree.evaluate - 1.0) * -1.0))
  }


  override def equals(other: Any): Boolean = {
      other match {
        case _: Variable => name.equals(other.toString)
        case _: Throwable => false
    }
  }

  override def toString: String = {
    if (value == null)
      return name
    value.toString
  }
}

object Variable {
  def apply(name: String): Variable = new Variable(name)
}