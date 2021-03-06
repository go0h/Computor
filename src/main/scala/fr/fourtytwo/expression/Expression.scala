package fr.fourtytwo.expression


trait Expression {

  def evaluate: Expression
  def changeSign: Expression
  def contains(op: String): Boolean

  def countVars: Int = distinctVars.size
  def distinctVars: Set[String] = {
    this match {
      case operator : Operator =>
        operator.getLeft.distinctVars ++ operator.getRight.distinctVars
      case v: Variable => {
        if (v.getName.equals("i")) Set()
        else Set(v.getName.toLowerCase)
      }
      case i: Indeterminate => Set(i.variable.getName.toLowerCase)
      case _ => Set()
    }
  }

  def setVar(varName: String, value: Expression): Expression = {
    this match {
      case o: Operator =>
        Operator(o.getLeft.setVar(varName, value), o.getOp, o.getRight.setVar(varName, value))
      case v: Variable =>
        if (v.getName.equalsIgnoreCase(varName)) value
        else this
      case i: Indeterminate =>
        if (i.variable.getName.equalsIgnoreCase(varName)) {
          Operator(i.constant, "*", Operator(value, "^", i.degree)).evaluate
        }
        else this
      case _ => this
    }
  }

  def getType: String = getClass.getSimpleName

  def toStringWithOrder: String = toString
}
