package fr.fourtytwo.token

import scala.util.matching.Regex


object TokenType extends Enumeration {
  val FUNCTION: Value = Value("([A-Za-z]+(?:\\s+)?[(][A-Za-z0-9,\\s]+[)])")
  val LITERAL: Value = Value("[A-Za-z]+(?:[0-9]+)?")
  val REALNUMBER: Value = Value("\\d+(?:\\.\\d+)?")
  val MATRIX: Value = Value("\\[[\\[0-9,;.\\-\\s\\]]+]")
  val SPACE: Value = Value("\\s+")
  val SEPARATOR: Value = Value(",")
  val OPERATION: Value = Value("[-\\+\\*/\\(\\)=^%]")
  val UNARY: Value = Value
  val NONE: Value = Value

  def createMatcher: Regex = {
    TokenType.values
      .toArray
      .map("(" + _.toString + ")")
      .mkString("|").r
  }

  def getMatchers: Map[TokenType.Value, Regex] = {
    TokenType.values
      .toArray
      .filter(x => x != NONE && x != UNARY)
      .map(x => (x, x.toString.r))
      .toMap
  }
}
