package fr.fourtytwo.expression.function

import fr.fourtytwo.exception.{EvaluateException, ParseException}
import fr.fourtytwo.expression.Expression

class UserDefinedFunction(name: String, varNames: Array[String], expr: Expression)
  extends Function(name) {

  if (varNames.length != expr.countVars) {
    throw new EvaluateException(
      s"""Function $name has ${varNames.length} arguments,
         |but ${(varNames.toSet -- expr.distinctVars).mkString(", ")} not uses""".stripMargin)
  }

  def apply(args: Expression*): Expression = {

    if (expr.countVars != args.length)
      throw new EvaluateException(s"Wrong num of args in $name, need ${varNames.length} but have ${args.length}")

    var res = expr
    for (x <- varNames.zip(args)) {
      res = res.setVar(x._1, x._2)
    }
    if (res.countVars != 0)
      throw new EvaluateException(s"Function $name, has ${res.countVars} initialized variables")
    res
  }

  override def numVars: Int = varNames.length

  override def toString: String = s"$name(${varNames.mkString(", ")}) = $expr"
}

object UserDefinedFunction {

  def apply(name: String = "<udf>", args: Array[String], expr: Expression): UserDefinedFunction = {
    new UserDefinedFunction(name, args, expr)
  }

}