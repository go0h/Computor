package fr.fourtytwo.expression

import fr.fourtytwo.exception.EvaluateException


class Variable extends Operable {

  private var sign: Int = 1
  private var name: String = _

  def this(varName: String) {
    this()
    if (varName.startsWith("-")) {
      sign = -1
      name = varName.substring(1)
    }
    else {
      name = varName
    }
  }

  def evaluate: Double = throw new EvaluateException(s"Variable $name has no value")
  def simplify: Expression = this

  def getSign: Int = sign
  def getName: String = name
  def changeSign: Variable = {
    if (sign == -1)
      return new Variable(name)
    Variable("-" + name)
  }
  def toIndeterminate: Indeterminate = new Indeterminate(sign, name, 1)

  ////////////////////////////////////////
  /////////// ADDITION METHODS ///////////
  ////////////////////////////////////////
  def +(other: RealNumber): Expression = {
    if (other.evaluate == 0.0)
      return this
    Operator(this, "+", other)
  }

  def +(other: Variable): Expression = {
    if (!name.equals(other.getName))
      throw new EvaluateException(s"Can't add different variables ($name, ${other.getName})")
    if (sign == other.getSign)
      return new Indeterminate(2 * sign, name, 1)
    new RealNumber(0)
  }

  def +(other: Indeterminate): Expression = {
    if (!name.equals(other.variable.getName))
      throw new EvaluateException(s"Can't add different variables ($name, ${other.variable.getName})")
    if (other.degree.evaluate == 1.0)
      return new Indeterminate(other.constant.evaluate + sign, name, 1)
    Operator(this, "+", other)
  }
  def +(other: ComplexNumber): Expression = Operator(this, "+", other)


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  def -(other: RealNumber): Expression = {
    if (other.evaluate == 0)
      return this
    Operator(this, "-", other)
  }

  def -(other: Variable): Expression = {
    if (!name.equals(other.getName))
      throw new EvaluateException(s"Can't sub different variables ($name, ${other.getName})")
    if (sign != other.sign)
      return new Indeterminate(sign * 2, name, 1)
    RealNumber(0)
  }

  def -(other: Indeterminate): Expression = {
    if (!name.equals(other.variable.getName))
      throw new EvaluateException(s"Can't sub different variables ($name, ${other.variable.getName})")
    if (other.degree.evaluate == 1) {
      if (other.constant.evaluate == 1)
        return RealNumber(0)
      return new Indeterminate(sign - other.constant.evaluate, name, other.degree.evaluate)
    }
    Operator(this, "-", other)
  }
  def -(other: ComplexNumber): Expression = Operator(this, "-", other)


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  def *(other: RealNumber): Indeterminate = new Indeterminate(other.evaluate * sign, name, 1.0)

  def *(other: Variable): Indeterminate = {
    if (!name.equals(other.getName))
      throw new EvaluateException(s"Can't multiply different variables ($name, ${other.getName})")
    new Indeterminate(sign * other.getSign, name, 2)
  }

  def *(other: Indeterminate): Indeterminate = {
    if (!name.equals(other.variable.getName))
      throw new EvaluateException(s"Can't multiply different variables ($name, ${other.variable.getName})")
    new Indeterminate(other.constant.evaluate * sign, name, other.degree.evaluate + 1)
  }
  def *(other: ComplexNumber): Expression = Operator(this, "*", other)


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  def /(other: RealNumber): Indeterminate = {
    new Indeterminate(1 / other.evaluate * sign, name, 1)
  }

  def /(other: Variable): Operable = {
    if (!name.equals(other.getName))
      throw new EvaluateException(s"Can't division different variables ($name, ${other.getName})")
    new RealNumber(sign / other.sign)
  }

  def /(other: Indeterminate): Operable = {
    if (!name.equals(other.variable.getName))
      throw new EvaluateException(s"Can't division different variables ($name, ${other.variable})")
    if (other.degree.evaluate == 1)
      return other.constant
    new Indeterminate(1 / other.constant.evaluate * sign,
                      name,
                      (other.degree.evaluate - 1) * -1)
  }
  def /(other: ComplexNumber): Expression = Operator(this, "/", other)


  ////////////////////////////////////////
  ///////////// POWER METHOD /////////////
  ////////////////////////////////////////
  override def ^(other: RealNumber): Expression = {
    if (other.evaluate == 0)
      return RealNumber(1.0)
    Indeterminate(RealNumber(1), this, RealNumber(other.evaluate))
  }


  ////////////////////////////////////////
  //////////// COMPARE METHOD ////////////
  ////////////////////////////////////////
  override def compare(other: Operable): Int = {
    other match {
      case _ : RealNumber => -1
      case v : Variable => {
        if (!equals(v))
          throw new EvaluateException(s"Can't compare different variables ($name, $v)")
        0
      }
      case i : Indeterminate => {
        if (!equals(i.variable))
          throw new EvaluateException(s"Can't compare different variables ($name, ${i.variable})")
        if (i.constant.evaluate == 1 && i.degree.evaluate == 1)
          return 0
        if (i.degree.evaluate < 1)
          return -1
        1
      }
      case _ : ComplexNumber => 1
    }
  }

  override def equals(other: Any): Boolean = {
      other match {
        case _: Variable => toString.equals(other.toString)
        case _ => false
    }
  }

  override def toString: String = if (sign == -1) s"-$name" else name

  def contains(op: String): Boolean = false
}

object Variable {
  def apply(name: String): Variable = new Variable(name)
}