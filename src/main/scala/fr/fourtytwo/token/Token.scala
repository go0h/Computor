package fr.fourtytwo.token

class Token(val expr: String, val tType: TokenType.Value) {

  override def toString: String = expr

  override def equals(other: Any): Boolean = {
    other match {
      case temp: Token => this.expr.equals(temp.expr) && this.tType == temp.tType
      case _ => false
    }
  }

  def ==(other: Token): Boolean = equals(other)

  def equals(other: String): Boolean = this.expr.equals(other)

}

object Token {
  def apply(expr: String, tokenType: TokenType.Value): Token = new Token(expr, tokenType)
}
