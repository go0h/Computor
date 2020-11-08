package fr.fourtytwo

import fr.fourtytwo.RPN.{convertToRPN, evaluate}
import fr.fourtytwo.exception.EvaluateException
import fr.fourtytwo.expression._

import scala.collection.mutable.{ArrayBuffer, Stack => ScalaStack}
import fr.fourtytwo.expression.Operator.priority
import fr.fourtytwo.token.{Token, TokenType}
import fr.fourtytwo.token.TokenType._

import scala.math.pow

class RPN(infixTokens: Array[Token]) {

  val tokens: Array[Token] = convertToRPN(infixTokens)

  def getTokens: Array[Token] = tokens

  override def toString: String = tokens.map(x => x.expr).mkString(" ")
  def infixString: String = infixTokens.map(x => x.expr).mkString("")

  def solve: Double = {

    val stack = ScalaStack[Double]()

    for (token <- tokens) {

      token.tType match {
        case REALNUMBER | NUMBER => {
          stack.push(token.expr.toDouble)
        }
        case OPERATION => {
          if (stack.length < 2)
            throw new EvaluateException(s"Wrong $infixString")
          stack.push(evaluate(stack.pop(), token.expr, stack.pop()))
        }
        case _ => throw new EvaluateException(s"Can't solve token: ${token.expr}")
      }
    }
    if (stack.length != 1)
      throw new EvaluateException(s"Wrong $infixString")
    stack.pop()
  }
}

object RPN {

  def apply(tokens: Array[Token]): RPN = new RPN(tokens)

  def convertToRPN(tokens: Array[Token]): Array[Token] = {

    val RPNTokens = ArrayBuffer[Token]()
    val stack = ScalaStack[Token]()

    var prev: Token = Token("", NONE)
    var prevNonSpace: Token = Token("", NONE)

    for (token <- tokens) {

      token.tType match {

        case OPERATION => {
          if (token.equals("-")                   // UNARY OPERATOR CONDITION
            && prev.tType >= SPACE && !prev.equals("-")
            && prevNonSpace.tType >= OPERATION && !prevNonSpace.equals(")")) {
              RPNTokens.append(Token("--", UNARY))
          }
          else if (stack.isEmpty || token.equals("(")) {
            stack.push(token)
          }
          else if (token.equals(")")) {
            while (!stack.head.equals("(")) {
              RPNTokens.append(stack.pop())
            }
            stack.pop()
          }
          else {
            while (stack.nonEmpty && priority(stack.head.expr) >= priority(token.expr)) {
              RPNTokens.append(stack.pop())
            }
            stack.push(token)
          }
          prevNonSpace = token
        }
        case NUMBER | REALNUMBER | VARIABLE => {
          RPNTokens.append(token)
          prevNonSpace = token
        }
        case _ =>

      }
      prev = token
    }
    RPNTokens.appendAll(stack)
    convertUnary(RPNTokens.toArray)
  }

  private def convertUnary(tokens: Array[Token]): Array[Token] = {

    val out: ArrayBuffer[Token] = new ArrayBuffer[Token]()
    var unary: Int = 1

    for (token <- tokens) {

      token.tType match {
        case UNARY => unary *= -1
        case _ => {
          if (unary == 1) {
            out.append(token)
          }
          else {
            out.append(Token("-" + token.expr, token.tType))
            unary = 1
          }
        }
      }
    }
    out.toArray
  }

  def evaluate(right: Double, op: String, left: Double): Double =  {
    op match {
      case "+" => left + right
      case "-" => left - right
      case "*" => left * right
      case "/" => {
        if (right == 0)
          throw new ArithmeticException("Division by zero")
        left / right
      }
      case "%" => {
        if (right == 0)
          throw new ArithmeticException("Modulo by zero")
        left % right
      }
      case "^" => pow(left, right)
    }
  }
}
