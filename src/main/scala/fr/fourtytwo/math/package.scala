package fr.fourtytwo

import fr.fourtytwo.exception.EvaluateException


package object math {

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

  def abs(d: Double): Double = if (d > 0) d else -d

  /** X0 = d
   *  Xn + 1 = 1/2(Xn + d / Xn)
   */
  def sqrt(d: Double): Double = {
    if (d < 0.0)
      return Double.NaN
    else if (abs(d) == 0.0)
      return d

    var prevRoot = 0.5 * (d + 1)
    var root = 0.5 * (prevRoot + d / prevRoot)
    while (root - prevRoot != 0) {
      prevRoot = root
      root = 0.5 * (prevRoot + d / prevRoot)
    }
    root
  }

  def pow(d: Double, n: Double): Double = {

    if (n.ceil != n)
      throw new EvaluateException("Can't power to non-Integer number")

    if (n == 0)
      return 1

    val m: Int = abs(n.toInt).toInt

    var res = d
    for (_ <- 1 until m) {
      res *= d
    }

    if (n < 0) 1.0 / res
    else res
  }

}
