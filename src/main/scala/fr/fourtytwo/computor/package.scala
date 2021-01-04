package fr.fourtytwo

import scala.util.matching.Regex
import fr.fourtytwo.token.{TokenType, Tokenizer}

package object computor {

  val MATCHERS: Map[TokenType.Value, Regex] = TokenType.getMatchers
  val TOKENIZER: Tokenizer = Tokenizer(MATCHERS)

  private val sp = "(?:\\s+)?"

  val VAR_ASSIGN_R: Regex = s"($sp[A-Za-z]+$sp)([=])(.*[^?])".r
  val VAR_COMP_R: Regex = s"($sp[A-Za-z]+$sp)([=]$sp[?]$sp)".r

  val FUNC_ASSIGN_R: Regex = s"($sp[A-Za-z]+$sp[(][A-Za-z\\s,]+[)]$sp)([=])(.*[^?])".r
  val FUNC_COMP_R: Regex = s"($sp[A-Za-z]+$sp[(][A-Za-z0-9\\s,.]+[)]$sp)([=]$sp[?]$sp)".r
  val POLY_FUNC_COMP_R: Regex = s"($sp[A-Za-z]+$sp[(][A-Za-z\\s,.]+[)]$sp)([=])($sp(?:[A-Za-z0-9.]+)?$sp[?]$sp)".r


  val COMMON_COMP: Regex = s"(.*[^?=])([=]$sp[?]$sp)".r

}
