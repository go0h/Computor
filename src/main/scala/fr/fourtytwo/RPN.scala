package fr.fourtytwo

import fr.fourtytwo.RPN.{convertToRPN, evaluate}

import scala.collection.mutable.{ArrayBuffer, Stack => ScalaStack}
import fr.fourtytwo.expression.{Operator, RealNumber}
import fr.fourtytwo.token.{Token, TokenType}

import scala.math.pow

class RPN {

  private var tokens: Array[Token] = _

  def this(infixTokens: Array[Token]) {
    this()
    tokens = convertToRPN(infixTokens)
  }
  def getTokens: Array[Token] = tokens

  def solve: Double = {

    val stack = ScalaStack[Double]()

    for (token <- tokens) {
      token.tokenType match {
        case TokenType.REALNUMBER | TokenType.NUMBER => {
          stack.push(token.expr.toDouble)
        }
        case TokenType.OPERATION => {
          if (stack.length < 2)
            throw new Exception(s"Wrong RPN ${tokens.map(x => x.expr).mkString(" ")}")

          stack.push(evaluate(stack.pop(), token.expr, stack.pop()))
        }
      }
    }
    if (stack.length != 1)
      throw new Exception(s"Wrong RPN ${tokens.map(x => x.expr).mkString(" ")}")
    stack.pop()
  }
}

object RPN {

  def apply(tokens: Array[Token]): RPN = new RPN(tokens)

  def convertToRPN(tokens: Array[Token]): Array[Token] = {

    val out = ArrayBuffer[Token]()
    val stack = ScalaStack[Token]()
    var filteredTokens = tokens.filter(x => x.tokenType != TokenType.SPACE)

    while (filteredTokens.nonEmpty) {

      val token = filteredTokens.head
      filteredTokens = filteredTokens.tail

      token.tokenType match {

        case TokenType.OPERATION => {
          if (stack.isEmpty || token.expr.equals("(")) {
            stack.push(token)
          }
          else if (token.expr.equals(")")) {
            while (!stack.head.expr.equals("(")) {
              out.append(stack.pop())
            }
            stack.pop()
          }
          else {
            if (Operator.priority(stack.head.expr) >= Operator.priority(token.expr)) {
              out.append(stack.pop())
            }
            stack.push(token)
          }
        }
        case TokenType.NUMBER | TokenType.REALNUMBER | TokenType.VARIABLE => {
          out.append(token)
        }
        case _ => println("Unknown")
      }
    }
    while (stack.nonEmpty) {
      out.append(stack.pop())
    }
    out.toArray
  }

  def evaluate(right: Double, op: String, left: Double): Double = {
    op match {
      case "+" => left + right
      case "-" => left - right
      case "*" => left * right
      case "/" => left / right
      case "^" => pow(left, right)
    }
  }
}
