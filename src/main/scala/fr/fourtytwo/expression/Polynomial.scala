package fr.fourtytwo.expression

import fr.fourtytwo.token.Token

class Polynomial(tokens: Array[Token]) extends Expression {

  override def evaluate: Double = ???
}

object Polynomial {
  def apply(): Polynomial = new Polynomial(Array[Token](null))
}