package fr.fourtytwo.token

import scala.util.matching.Regex

object TokenType extends Enumeration {

  val INDETERMINATE: Value = Value("""^(-?\d+(?:\.\d+)?)([*\\])(-?[A-Za-z]+)\^(\d+(?:\.\d+)?)""")
  val VARIABLE: Value = Value("[A-Za-z]+(?:[0-9]+)?")
  val REALNUMBER: Value = Value("\\d+(?:\\.\\d+)?")
//  val NUMBER: Value = Value("[0-9]+")
  val SPACE: Value = Value("\\s+")
  val OPERATION: Value = Value("[-\\+\\*/\\(\\)=^%]")
  val UNARY: Value = Value
  val NONE: Value = Value

  def createMatcher: Regex = {
    TokenType.values
      .toArray
      .map("(" + _.toString + ")")
      .mkString("|").r
  }

  def getSimpleTypesWithRegex: Map[TokenType.Value, Regex] = {
    TokenType.values
      .toArray
      .filter(x => x != INDETERMINATE && x != NONE && x != UNARY)
      .map(x => (x, x.toString.r))
      .toMap
  }

  def getAllTypesWithRegex: Map[TokenType.Value, Regex] = {
    TokenType.values
      .toArray
      .map(x => (x, x.toString.r))
      .toMap
  }

}
