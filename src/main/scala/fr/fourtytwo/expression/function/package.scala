package fr.fourtytwo.expression

import scala.collection.immutable.Map
import fr.fourtytwo.math

package object function {

  val abs: function.Function1 = Function1("abs", math.abs)
  val sqrt: function.Function1 = Function1("sqrt", math.sqrt)

  import fr.fourtytwo.expression.Implicit._

  val add: UserDefinedFunction = UserDefinedFunction("add", Array("a", "b"), Operator("a", "+", "b"))
  val sub: UserDefinedFunction = UserDefinedFunction("sub", Array("a", "b"), Operator("a", "-", "b"))
  val mul: UserDefinedFunction = UserDefinedFunction("mul", Array("a", "b"), Operator("a", "*", "b"))
  val div: UserDefinedFunction = UserDefinedFunction("div", Array("a", "b"), Operator("a", "/", "b"))
  val mod: UserDefinedFunction = UserDefinedFunction("mod", Array("a", "b"), Operator("a", "%", "b"))
  val pow: UserDefinedFunction = UserDefinedFunction("pow", Array("a", "b"), Operator("a", "^", "b"))

  def getPredefFunctions: Map[String, Function] = {
    Array(abs, sqrt, add, sub, mul, div, mod, pow).map(x => (x.getName, x)).toMap
  }
}
