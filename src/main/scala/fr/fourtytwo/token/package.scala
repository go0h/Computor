package fr.fourtytwo

import scala.util.matching.Regex

package object token {

  val MATCHERS: Map[TokenType.Value, Regex] = TokenType.getSimpleTypesWithRegex
  val SIMPLE_TOKENIZER: Tokenizer = Tokenizer(MATCHERS)

  val INDETER_MATCHERS: Map[TokenType.Value, Regex] = TokenType.getAllTypesWithRegex
    .filter(x => x._1 != TokenType.VARIABLE
      && x._1 != TokenType.UNARY
      && x._1 != TokenType.NONE)
  val INDETER_TOKENIZER: Tokenizer = Tokenizer(INDETER_MATCHERS)

}
