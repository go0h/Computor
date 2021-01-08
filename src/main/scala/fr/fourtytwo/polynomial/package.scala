package fr.fourtytwo

import fr.fourtytwo.token.{TokenType, Tokenizer}

import scala.util.matching.Regex


package object polynomial {

  val MATCHERS: Map[TokenType.Value, Regex] = TokenType.getMatchers
    .filter(x => x._1 != TokenType.MATRIX)

  val SIMPLE_TOKENIZER: Tokenizer = Tokenizer(MATCHERS)

  val USAGE: String =
    """Usage: ./ComputorV1 "expression" [-d]
      |   -h, --help			display this help and exit
      |   -d, --debug			debug mode""".stripMargin

}
