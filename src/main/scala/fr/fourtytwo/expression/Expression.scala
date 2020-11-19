package fr.fourtytwo.expression

trait Expression {
  def evaluate: Double
  def optimize: Expression
  def changeSign: Expression
}
