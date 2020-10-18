package fr.fourtytwo.expression

class Polynomial extends Expression {

  override def evaluate: Double = ???
}

object Polynomial {
  def apply(): Polynomial = new Polynomial()
}