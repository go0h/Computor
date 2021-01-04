package fr.fourtytwo.expression.function

import fr.fourtytwo.exception.EvaluateException
import fr.fourtytwo.expression.{Expression, RealNumber}

class Function1 private[expression](name: String, func: Double => Double)
  extends Function(name) {

  def apply(args: Expression*): Expression = {

    if (args.length != 1) {
      throw new EvaluateException(s"Wrong num of args in $name, need 1 but have ${args.length}")
    }

    val arg = args.head
    if (!arg.isInstanceOf[RealNumber])
      throw new EvaluateException(s"Wrong argument in function '$name' need 'RealNumber'," +
        s" but have '${arg.getType}'")
    RealNumber(func(arg.asInstanceOf[RealNumber].getNum))
  }

   def numVars: Int = 1
}

object Function1 {
  private[expression] def apply(name: String, func: Double => Double): Function1 = {
    new Function1(name, func)
  }
}