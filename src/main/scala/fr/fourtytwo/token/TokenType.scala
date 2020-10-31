package fr.fourtytwo.token

import scala.util.matching.Regex

object TokenType extends Enumeration {

  val VARIABLE: Value = Value("[A-Za-z]+(?:[0-9]+)?")
  val REALNUMBER: Value = Value("[0-9]+\\.[0-9]+")
  val NUMBER: Value = Value("[0-9]+")
  val SPACE: Value = Value("\\s+")
  val OPERATION: Value = Value("[-\\+\\*/\\(\\)=^]")

  def createMatcher: Regex = {
    TokenType.values.toArray.map("(" + _.toString + ")").mkString("|").r
  }

  def getTypesWithRegex: Map[TokenType.Value, Regex] = {
    TokenType.values.toArray.map(x => (x, x.toString.r)).toMap
  }
}
