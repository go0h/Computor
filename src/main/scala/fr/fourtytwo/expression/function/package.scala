package fr.fourtytwo.expression

import fr.fourtytwo.math

import scala.collection.immutable.Map

package object function {

  val abs: function.Function1 = Function1("abs", math.abs)
  val sin: function.Function1 = Function1("sin", scala.math.sin)
  val cos: function.Function1 = Function1("cos", scala.math.cos)
  val tan: function.Function1 = Function1("tan", scala.math.tan)
  val asin: function.Function1 = Function1("asin", scala.math.asin)
  val acos: function.Function1 = Function1("acos", scala.math.acos)
  val sqrt: function.Function1 = Function1("sqrt", math.sqrt)
  val cbrt: function.Function1 = Function1("cbrt", scala.math.cbrt)

  import fr.fourtytwo.expression.Implicit._

  val add: UserDefinedFunction = UserDefinedFunction("add", Array("a", "b"), Operator("a", "+", "b"))
  val sub: UserDefinedFunction = UserDefinedFunction("sub", Array("a", "b"), Operator("a", "-", "b"))
  val mul: UserDefinedFunction = UserDefinedFunction("mul", Array("a", "b"), Operator("a", "*", "b"))
  val div: UserDefinedFunction = UserDefinedFunction("div", Array("a", "b"), Operator("a", "/", "b"))
  val mod: UserDefinedFunction = UserDefinedFunction("mod", Array("a", "b"), Operator("a", "%", "b"))
  val pow: UserDefinedFunction = UserDefinedFunction("pow", Array("a", "b"), Operator("a", "^", "b"))

  def getPredefFunctions: Map[String, Function] = {
    Array(abs, sin, cos, tan, asin, acos, sqrt, cbrt, add, sub, mul, div, mod, pow)
      .map(x => (x.getName, x)).toMap
  }
}
