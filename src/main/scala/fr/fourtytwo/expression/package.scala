package fr.fourtytwo

import scala.collection.immutable.Map
import fr.fourtytwo.expression.function._


package object expression {

  def division(a: Double, b: Double): Double = {
    if (b == 0)
      throw new ArithmeticException(s"Division by zero ($a / $b)")
    a / b
  }

  def modulo(a: Double, b: Double): Double = {
    if (b == 0)
      throw new ArithmeticException(s"Modulo by zero ($a % $b)")
    a % b
  }

  val OPERATORS: Map[String, (Double, Double) => Double] =
    Map("+" -> (_ + _), "-" -> (_ - _), "*" -> (_ * _),
      "/" -> division, "%" -> modulo, "^" -> math.pow)

  val abs: function.Function1 = Function1("abs", math.abs)
  val sin: function.Function1 = Function1("sin", math.sin)
  val cos: function.Function1 = Function1("cos", math.cos)
  val tan: function.Function1 = Function1("tan", math.tan)
  val asin: function.Function1 = Function1("asin", math.asin)
  val acos: function.Function1 = Function1("acos", math.acos)
  val sqrt: function.Function1 = Function1("sqrt", math.sqrt)
  val cbrt: function.Function1 = Function1("cbrt", math.cbrt)

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
