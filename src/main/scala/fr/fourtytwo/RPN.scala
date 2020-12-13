package fr.fourtytwo

import scala.collection.mutable.{ArrayBuffer, Stack => ScalaStack}
import fr.fourtytwo.exception.EvaluateException
import fr.fourtytwo.expression._
import fr.fourtytwo.expression.Operator.priority
import fr.fourtytwo.token.Token
import fr.fourtytwo.token.TokenType._


class RPN(infixTokens: Array[Token]) {

  val tokens: Array[Token] = RPN.convertToRPN(infixTokens)

  def getTokens: Array[Token] = tokens

  override def toString: String = tokens.map(x => x.expr).mkString(" ")
  def infixString: String = infixTokens.map(x => x.expr).mkString("")

  def solve: Expression = {

    val stack = ScalaStack[Expression]()

    for (token <- tokens) {

      token.tType match {
        case REALNUMBER => stack.push(RealNumber(token.expr.toDouble))
        case VARIABLE => stack.push(Variable(token.expr))
        case OPERATION =>
          if (token.equals("--")) {
            stack.push(Operator(RealNumber(0), "-", stack.pop()))
          }
          else {
            if (stack.length < 2)
              throw new EvaluateException(s"Wrong stack length: $infixString")
            val first = stack.pop()
            stack.push(Operator(stack.pop(), token.expr, first))
          }
        case _ => throw new EvaluateException(s"Can't solve token: ${token.expr}")
      }
    }
    if (stack.length != 1)
      throw new EvaluateException(s"Wrong stack length in the end: $infixString")
    stack.pop()
  }
}

object RPN {

  def apply(tokens: Array[Token]): RPN = new RPN(tokens)

  def convertToRPN(tokens: Array[Token]): Array[Token] = {

    val RPNTokens = ArrayBuffer[Token]()
    val stack = ScalaStack[Token]()

    var mayUnary: Boolean = true
    var prevToken: Token = Token("", NONE)

    for (token <- tokens) {

      token.tType match {

        case OPERATION =>
          if (token.equals("(")) {
            stack.push(token)
            mayUnary = true
          }
          else if (token.equals(")")) {
            while (!stack.head.equals("(")) {
              RPNTokens.append(stack.pop())
            }
            stack.pop()
            mayUnary = false
          }
          else {
            val tempToken =
              if (mayUnary && token.equals("-") && !prevToken.equals("-"))
                Token("--", token.tType)
              else token

            while (stack.nonEmpty && priority(stack.head.expr) >= priority(tempToken.expr)) {
              RPNTokens.append(stack.pop())
            }
            stack.push(tempToken)
            mayUnary = if (prevToken.equals("-")) false else true
          }
        case REALNUMBER | VARIABLE =>
          RPNTokens.append(token)
          mayUnary = false
        case _ =>
      }
      prevToken = token
    }
    RPNTokens.appendAll(stack).toArray
  }
}
