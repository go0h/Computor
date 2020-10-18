package fr.fourtytwo.parser

import fr.fourtytwo.exception.ParseException

import scala.collection.mutable

class Parser(stringExpr: String) {
  val expression: String = stringExpr.replaceAll("\\s+", "")

  basicCheckBrackets()

  def basicCheckBrackets(): Unit = {
    val stack = mutable.Stack[Char]()

    for (i <- 0 until expression.length) {
      if (expression(i) == '(')
        stack.push(expression(i))
      else if (expression(i) == ')') {
        if (stack.nonEmpty && stack(0) == '(')
          stack.pop()
        else
          throw new ParseException(s"Brackets error: $expression", expression.length - i)
      }
    }
    if (stack.nonEmpty)
      throw new ParseException(s"Brackets error: $expression")
  }
}

object Parser {
  def apply(expression: String): Parser = {
    new Parser(expression)
  }
}