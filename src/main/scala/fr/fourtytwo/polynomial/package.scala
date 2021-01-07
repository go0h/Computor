package fr.fourtytwo

import scala.util.matching.Regex
import fr.fourtytwo.token.TokenType._
import fr.fourtytwo.token.Tokenizer


package object polynomial {

  val MATCHERS: Map[Value, Regex] = getMatchers
    .filter(x => x._1 != MATRIX && x._1 != SEPARATOR)

  val SIMPLE_TOKENIZER: Tokenizer = Tokenizer(MATCHERS)

  val USAGE: String =
    """Usage: ./ComputorV1 "expression" [-d]
      |   -h, --help			display this help and exit
      |   -d, --debug			debug mode""".stripMargin

}
