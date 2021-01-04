package fr.fourtytwo.expression


class Variable extends Operable {

  private var sign: Int = 1
  private var name: String = _

  def this(varName: String) = {
    this()
    if (varName.startsWith("-")) {
      sign = -1
      name = varName.substring(1)
    }
    else {
      name = varName
    }
  }

  def evaluate: Expression = this
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
  override def +(other: RealNumber): Expression = {
    if (other == 0.0)
      return this
    Operator(this, "+", other)
  }

  override def +(other: Variable): Expression = {
    if (!name.equalsIgnoreCase(other.getName))
      return Operator(this, "+", other)
    if (sign == other.getSign)
      return new Indeterminate(2 * sign, name, 1)
    new RealNumber(0)
  }

  override def +(other: Indeterminate): Expression = {
    if (!name.equalsIgnoreCase(other.variable.getName))
      return Operator(this, "+", other)
    if (other.degree == 1.0)
      return new Indeterminate(other.constant + sign, Variable(name), RealNumber(1))
    Operator(this, "+", other)
  }
  override def +(other: ComplexNumber): Expression = Operator(this, "+", other)


  ////////////////////////////////////////
  ////////// SUBTRACTION METHODS /////////
  ////////////////////////////////////////
  override def -(other: RealNumber): Expression = {
    if (other == 0)
      return this
    Operator(this, "-", other)
  }

  override def -(other: Variable): Expression = {
    if (!name.equalsIgnoreCase(other.getName))
      return Operator(this, "-", other)
    if (sign != other.sign)
      return new Indeterminate(sign * 2, name, 1)
    RealNumber(0)
  }

  override def -(other: Indeterminate): Expression = {
    if (!name.equalsIgnoreCase(other.variable.getName))
      return Operator(this, "-", other)
    if (other.degree == 1) {
      if (other.constant == 1)
        return RealNumber(0)
      return new Indeterminate(sign - other.constant.getNum, name, other.degree.getNum)
    }
    Operator(this, "-", other)
  }
  override def -(other: ComplexNumber): Expression = {
    Operator(this, "-", other)
  }


  ////////////////////////////////////////
  //////////// MULTIPLY METHODS //////////
  ////////////////////////////////////////
  override def *(other: RealNumber): Expression = {
    if (other == 0)
      return RealNumber(0)
    if (name.equalsIgnoreCase("i"))
      return ComplexNumber(0, other.getNum * sign)
    Indeterminate(other * sign, Variable(name), RealNumber(1))
  }

  override def *(other: Variable): Expression = {
    if (!name.equalsIgnoreCase(other.getName))
      return Operator(this, "*", other)
    new Indeterminate(sign * other.getSign, name, 2)
  }

  override def *(other: Indeterminate): Expression = other * this
  override def *(other: ComplexNumber): Expression = Operator(this, "*", other)


  ////////////////////////////////////////
  //////////// DIVISION METHODS //////////
  ////////////////////////////////////////
  override def /(other: RealNumber): Indeterminate = {
    new Indeterminate(1 / other.getNum * sign, name, 1)
  }

  override def /(other: Variable): Expression = {
    if (!name.equalsIgnoreCase(other.getName))
      return Operator(this, "/", other)
    new RealNumber(sign / other.sign)
  }

  override def /(other: Indeterminate): Expression = {
    if (!name.equalsIgnoreCase(other.variable.getName))
      return Operator(this, "/", other)
    if (other.degree == 1)
      return other.constant
    new Indeterminate(1 / other.constant.getNum * sign,
                      name,
                      (other.degree.getNum - 1) * -1)
  }
  override def /(other: ComplexNumber): Expression = Operator(this, "/", other)


  ////////////////////////////////////////
  ///////////// POWER METHOD /////////////
  ////////////////////////////////////////
  override def ^(other: RealNumber): Expression = {
    if (other == 0)
      return RealNumber(1)
    Indeterminate(RealNumber(1), this, other)
  }


  ////////////////////////////////////////
  //////////// COMPARE METHOD ////////////
  ////////////////////////////////////////
  override def compare(other: Operable): Int = {
    other match {
      case _ : RealNumber => -1
      case _ : ComplexNumber => -1
      case _ : Variable => 0
      case i : Indeterminate => {
        if (i.constant == 1 && i.degree == 1)
          return 0
        if (i.degree.getNum < 1)
          return -1
        1
      }
      case _ : Matrix => 1
    }
  }

  override def equals(other: Any): Boolean = {
    other match {
      case v: Variable => name.equalsIgnoreCase(v.getName)
      case _ => false
    }
  }

  override def toString: String = if (sign == -1) s"-$name" else name

  def contains(op: String): Boolean = false
}

object Variable {
  def apply(name: String): Variable = new Variable(name)
}