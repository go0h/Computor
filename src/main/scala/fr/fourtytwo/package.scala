package fr

import fr.fourtytwo.token.{TokenType, Tokenizer}

import scala.util.matching.Regex

package object fourtytwo {

  val MATCHERS: Map[TokenType.Value, Regex] = TokenType.getSimpleTypesWithRegex
  val TOKENIZER: Tokenizer = Tokenizer(MATCHERS)

}
