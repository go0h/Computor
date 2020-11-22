package fr.fourtytwo

import fr.fourtytwo.token.{TokenType, Tokenizer}

import scala.util.matching.Regex

package object expression {

  val MATCHERS: Map[TokenType.Value, Regex] = TokenType.getMatchers
  val SIMPLE_TOKENIZER: Tokenizer = Tokenizer(MATCHERS)
}
