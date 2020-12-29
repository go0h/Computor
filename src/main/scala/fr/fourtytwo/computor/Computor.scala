package fr.fourtytwo.computor

import fr.fourtytwo.computor.Computor.basicCheck

import scala.io.StdIn
import scala.collection.mutable.{ArrayBuffer, Map => MMap, Stack => ScalaStack}
import fr.fourtytwo.exception.{EvaluateException, ParseException}
import fr.fourtytwo.expression._
import fr.fourtytwo.expression.Operator._
import fr.fourtytwo.expression.function._
import fr.fourtytwo.token.Token
import fr.fourtytwo.token.TokenType._

class Computor {

  var infixTokens: Array[Token] = _
  var tokens: Array[Token] = _

  def this(infix: Array[Token]) = {
    this()
    infixTokens = infix
    tokens = convertToRPN(infixTokens)
  }

  def run(): Unit = {

    var line = StdIn.readLine()

    while (line != null) {

      try {

        if (line.equals("q") || line.equals("quit")) {
          System.exit(0)
        }
        else {
          basicCheck(line)
          line match {
            case VAR_ASSIGN_R(varName, _, expr) => println(s"|$varName = $expr| is variable definition")
            case VAR_COMP_R(varName, _) => println(s"|$varName| is variable computation")
            case FUNC_ASSIGN_R(func, _, expr) => println(s"|$func = $expr| is function definition")
            case FUNC_COMP_R(func, _, expr) => println(s"|$func = $expr| is function computation")
            case COMMON_COMP(expr, _) => println(s"|$expr| is common computation")
            case _ => throw new ParseException(s"Can't recognize expression type $line")
          }
        }
      } catch {
        case ex: ParseException => println(ex.getMessage)
        case ex: EvaluateException => println(ex.getMessage)
        case ex: Exception => println(s"${ex.getClass.getName} ${ex.getMessage}")
      }
      line = StdIn.readLine()
    }
  }

  def getTokens: Array[Token] = tokens

  override def toString: String = tokens.map(x => x.expr).mkString(" ")
  def infixString: String = infixTokens.map(x => x.expr).mkString("")

  val vars: MMap[String, Expression] = MMap[String, Expression]()
  val funcs: MMap[String, Function] = MMap[String, Function]() ++ getPredefFunctions

  def solve: Expression = {
    if (tokens == null)
      throw new EvaluateException("Expression is not defined")
    solve(tokens)
  }

  def solve(tokens: Array[Token]): Expression = {

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

  def basicCheck(expr: String): Unit = {

    if (expr.count(_ == '=') != 1)
      throw new ParseException(s"Expression $expr has ${expr.count(_ == '=')} equal sign")

    val exprSplit = expr.split("=", 2)

    val (left, right) = (exprSplit(0), exprSplit(1))

    if (left.trim.isEmpty)
      throw new ParseException(s"Left expression is empty")

    if (right.trim.isEmpty)
      throw new ParseException(s"Right expression is empty")

  }
}
