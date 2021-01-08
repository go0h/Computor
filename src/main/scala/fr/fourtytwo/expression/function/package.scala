package fr.fourtytwo.expression

import scala.collection.immutable.Map
import fr.fourtytwo.math


package object function {

  def toE2E(f: Double => Double)(expr: Expression): Expression = {
    RealNumber(f(expr.asInstanceOf[RealNumber].getNum))
  }

  val abs: function.Function1 = Function1("abs", toE2E(math.abs))
  val sin: function.Function1 = Function1("sin", toE2E(scala.math.sin))
  val cos: function.Function1 = Function1("cos", toE2E(scala.math.cos))
  val tan: function.Function1 = Function1("tan", toE2E(scala.math.tan))
  val asin: function.Function1 = Function1("asin", toE2E(scala.math.asin))
  val acos: function.Function1 = Function1("acos", toE2E(scala.math.acos))
  val cbrt: function.Function1 = Function1("cbrt", toE2E(scala.math.cbrt))
  val log: function.Function1 = Function1("log", toE2E(scala.math.log))
  val fact: function.Function1 = Function1("fact", toE2E(math.fact))
  val sqrt: function.Function1 = Function1("sqrt", math.sqrtComplex)

  import fr.fourtytwo.expression.Implicit._

  val add: UserDefinedFunction = UserDefinedFunction("add", Array("a", "b"), Operator("a", "+", "b"))
  val sub: UserDefinedFunction = UserDefinedFunction("sub", Array("a", "b"), Operator("a", "-", "b"))
  val mul: UserDefinedFunction = UserDefinedFunction("mul", Array("a", "b"), Operator("a", "*", "b"))
  val div: UserDefinedFunction = UserDefinedFunction("div", Array("a", "b"), Operator("a", "/", "b"))
  val mod: UserDefinedFunction = UserDefinedFunction("mod", Array("a", "b"), Operator("a", "%", "b"))
  val pow: UserDefinedFunction = UserDefinedFunction("pow", Array("a", "b"), Operator("a", "^", "b"))

  def getPredefFunctions: Map[String, Function] = {
    Array(abs, sin, cos, tan, asin, acos, fact, cbrt, log, add, sub, mul, div, mod, pow, sqrt)
      .map(x => (x.getName, x)).toMap
  }

  def getPredefVariables: Map[String, RealNumber] = Map("pi" -> scala.math.Pi, "e" -> scala.math.E)
}
