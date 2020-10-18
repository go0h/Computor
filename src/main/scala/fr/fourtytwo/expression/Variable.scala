package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException

class Variable(name: String, value: Number = null) extends Expression {

  override def evaluate: Double = {
    if (value == null)
      throw new EvaluateException(s"Variable $value has no value")
    value.evaluate
  }

  override def toString: String = {
    if (value == null)
      return name
    value.toString
  }
}

object Variable {
  def apply(name: String): Variable = {
    new Variable(name)
  }
}