package fr.fourtytwo.expression.function

import fr.fourtytwo.expression.Expression


abstract class Function(name: String) extends Expression {

  def apply(args: Expression*): Expression

  def numVars: Int

  def getName: String = name

  override def toString: String = name

  override def evaluate: Expression = this
  override def changeSign: Expression = this
  override def contains(op: String): Boolean = false
}