package fr.fourtytwo.expression

trait Expression {
  def evaluate: Double
  def simplify: Expression
  def changeSign: Expression
}
