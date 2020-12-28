package fr.fourtytwo.computor

import scala.collection.mutable.{ArrayBuffer, Map => MMap, Stack => ScalaStack}
import fr.fourtytwo.exception.EvaluateException
import fr.fourtytwo.expression._
import fr.fourtytwo.expression.Operator._
import fr.fourtytwo.expression.function._
import fr.fourtytwo.token.Token
import fr.fourtytwo.token.TokenType._

class Computor(infixTokens: Array[Token]) {

  val tokens: Array[Token] = convertToRPN(infixTokens)

  def getTokens: Array[Token] = tokens

  override def toString: String = tokens.map(x => x.expr).mkString(" ")
  def infixString: String = infixTokens.map(x => x.expr).mkString("")

  val vars: MMap[String, Expression] = MMap[String, Expression]()
  val funcs: MMap[String, Function] = MMap[String, Function]() ++ getPredefFunctions

  def solve: Expression = {

    val stack = ScalaStack[Expression]()
    var i = 0

    while (i < tokens.length) {

      tokens(i).tType match {
        case REALNUMBER => stack.push(RealNumber(tokens(i).expr.toDouble))
        case LITERAL => {
          if (funcs.contains(tokens(i).expr)) {
            val (f, k) = setFuncParams(funcs(tokens(i).expr), i + 1)
            i = k
            stack.push(f)
          }
          else
            stack.push(Variable(tokens(i).expr))
        }
        case OPERATION =>
            if (stack.length < 2)
              throw new EvaluateException(s"Wrong stack length: $infixString")
            val first = stack.pop()
            stack.push(Operator(stack.pop(), tokens(i).expr, first))
        case UNARY => stack.push(Operator(RealNumber(0), "-", stack.pop()))
        case _ => throw new EvaluateException(s"Can't solve token: ${tokens(i).expr}")
      }
      i += 1
    }
    if (stack.length != 1)
      throw new EvaluateException(s"Wrong stack length in the end: $infixString")
    stack.pop()
  }

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
                Token("--", UNARY)
              else token

            while (stack.nonEmpty
              && ((leftAssoc(stack.head.expr) && priority(stack.head.expr) >= priority(tempToken.expr))
              || (!leftAssoc(stack.head.expr) && priority(stack.head.expr) > priority(tempToken.expr)))) {
              RPNTokens.append(stack.pop())
            }
            stack.push(tempToken)
            mayUnary = if (prevToken.equals("-")) false else true
          }
        case REALNUMBER | LITERAL =>
          RPNTokens.append(token)
          if (stack.nonEmpty && stack.head.tType == UNARY)
            RPNTokens.append(stack.pop())
          mayUnary = false
        case SEPARATOR => mayUnary = true
        case _ =>
      }
      prevToken = token
    }
    RPNTokens.appendAll(stack).toArray
  }

  def setFuncParams(func: Function, i: Int): (Expression, Int) = {

    if (i == tokens.length)
      throw new EvaluateException(s"Function ${func.getName} has no arguments")

    var j = 0
    var k = i
    var args = Seq[Expression]()

    while (j < func.numVars && k < tokens.length) {
      tokens(k).tType match {
        case REALNUMBER =>
          args +:= RealNumber(tokens(k).expr.toDouble)
          j += 1
        case LITERAL if vars.contains(tokens(k).expr) =>
          args +:= vars(tokens(k).expr)
          j += 1
        case UNARY if args.nonEmpty =>
          args = Seq(args.head.changeSign) ++ args.tail
        case _ =>
          throw new EvaluateException(s"Wrong argument ${tokens(k)} in function ${func.getName}")
      }
      k += 1
    }
    if (tokens(k).tType == UNARY) {
      args = Seq(args.head.changeSign) ++ args.tail
      k += 1
    }
    (func(args.reverse:_*), k - 1)
  }
}

object Computor {
  def apply(tokens: Array[Token]): Computor = new Computor(tokens)
}
