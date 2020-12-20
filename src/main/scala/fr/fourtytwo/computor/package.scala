package fr.fourtytwo

import scala.util.matching.Regex
import fr.fourtytwo.token.{TokenType, Tokenizer}

package object computor {

  val MATCHERS: Map[TokenType.Value, Regex] = TokenType.getMatchers
  val SIMPLE_TOKENIZER: Tokenizer = Tokenizer(MATCHERS)

}
