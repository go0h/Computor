package fr.fourtytwo.token

class Token(val expr: String, val tType: TokenType.Value) {
  override def toString: String = expr

  override def equals(other: Any): Boolean = {
    if (other.isInstanceOf[Token]) {
      val temp = other.asInstanceOf[Token]
      return this.expr.equals(temp.expr) && this.tType == temp.tType
    }
    false
  }

  def ==(other: Token): Boolean = equals(other)

}

object Token {
  def apply(expr: String, tokenType: TokenType.Value): Token = new Token(expr, tokenType)
}
