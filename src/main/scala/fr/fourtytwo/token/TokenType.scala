package fr.fourtytwo.token

import scala.util.matching.Regex


object TokenType extends Enumeration {

  val LITERAL: Value = Value("[A-Za-z]+(?:[0-9]+)?")
  val REALNUMBER: Value = Value("\\d+(?:\\.\\d+)?")
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
