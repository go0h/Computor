package fr.fourtytwo

import scala.util.matching.Regex
import fr.fourtytwo.token.{TokenType, Tokenizer}


package object polynomial {

  val MATCHERS: Map[TokenType.Value, Regex] = TokenType.getMatchers
  val SIMPLE_TOKENIZER: Tokenizer = Tokenizer(MATCHERS)

  val USAGE: String =
    """Usage: ./ComputorV1 "expression" [-d]
      |   -h, --help			display this help and exit
      |   -d, --debug			debug mode""".stripMargin

}
