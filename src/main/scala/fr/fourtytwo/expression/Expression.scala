package fr.fourtytwo.expression


trait Expression {
  def evaluate: Double
  def simplify: Expression
  def changeSign: Expression
  def contains(op: String): Boolean
}
