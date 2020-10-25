package fr.fourtytwo.token

class Token(val expr: String, val tokenType: TokenType.Value) {
  override def toString: String = expr
}

object Token {
  def apply(expr: String, tokenType: TokenType.Value): Token = {
    new Token(expr, tokenType)
  }
}
