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

//TODO
// Валидацию функций
// Исполнение функций
// Вычисление полиномиальных функций
class Computor {

  var infixTokens: Array[Token] = _
  var tokens: Array[Token] = _

  val vars: MMap[String, Expression] = MMap[String, Expression]()
  val funcs: MMap[String, Function] = MMap[String, Function]() ++ getPredefFunctions

  def this(infix: Array[Token]) = {
    this()
    infixTokens = infix
    tokens = convertToRPN(infixTokens)
  }

  override def toString: String = infixTokens.map(x => x.expr).mkString(" ")

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
            case VAR_ASSIGN_R(varName, _, expr) => variableAssignment(varName, expr)
            case VAR_COMP_R(varName, _) => variableComputation(varName)
            case FUNC_ASSIGN_R(func, _, expr) => functionAssignment(func, expr)
            case FUNC_COMP_R(func, _, expr) => functionComputation(func, expr)
            case COMMON_COMP(expr, _) => commonComputation(expr)
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
        case MATRIX => stack.push(Matrix(tokens(i).expr))
        case LITERAL => stack.push(Variable(tokens(i).expr))
        case FUNCTION => stack.push(setFunction(tokens(i).expr))
        case OPERATION =>
            if (stack.length < 2)
              throw new EvaluateException(s"Wrong stack length: $toString")
            val first = stack.pop()
            stack.push(Operator(stack.pop(), tokens(i).expr, first))
        case UNARY => stack.push(Operator(RealNumber(0), "-", stack.pop()))
        case _ => throw new EvaluateException(s"Can't solve token: ${tokens(i).expr}")
      }
      i += 1
    }
    if (stack.length != 1)
      throw new EvaluateException(s"Wrong stack length in the end: $toString")
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
        case REALNUMBER | LITERAL | MATRIX | FUNCTION =>
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

  def variableAssignment(varName: String, expression: String): Unit = {

    println(s"|${varName.trim} = ${expression.trim}| is variable assignment")

    var res = createExpression(expression)
    if (res.countVars != 0) {
      res = serVariablesToExpr(res)
    }

    res = res.evaluate
    vars(varName.trim) = res

    println(res)
  }

  def variableComputation(varName: String): Unit = {

    println(s"|$varName| is variable computation")

    val res = vars.getOrElse(varName.trim, throw new EvaluateException(s"Unknown variable '$varName'"))

    println(res)
  }

  def functionAssignment(funcWithParams: String, expression: String): Unit = {

    println(s"|$funcWithParams = $expression| is function assignment")

    val expr = createExpression(expression)

    val funcName = funcWithParams
      .substring(0, funcWithParams.indexOf("(")).trim

    val argString = funcWithParams
      .substring(funcWithParams.indexOf("(") + 1, funcWithParams.lastIndexOf(")"))

    val arguments = TOKENIZER.generateTokens(argString).filter(_.tType != SPACE)

    validateFuncParams(funcName, arguments, expr.countVars)

    val funcArgs = arguments.filter(x => x.tType != SEPARATOR).map(_.expr)

    if ((expr.distinctVars -- funcArgs).nonEmpty)
      throw new ParseException(s"Function '$funcName' expected params '${funcArgs.mkString(", ")}', " +
        s"but have '${expr.distinctVars.mkString(", ")}'")

    val func = UserDefinedFunction(funcName, funcArgs, expr)

    funcs(funcName) = func

    println(func)
  }


  def functionComputation(funcWithParams: String, expr: String): Unit = {

    if (!expr.trim.equals("?"))
      throw new EvaluateException("Polynomial")

    val res = setFunction(funcWithParams).evaluate
    println(res)
  }

  def commonComputation(expression: String): Unit = {

    println(s"|${expression.trim}| is common computation")

    var res = createExpression(expression)
    println(tokens.map(_.expr).mkString(" "))

    if (res.countVars != 0) {
      res = serVariablesToExpr(res)
    }
    res = res.evaluate

    println(res)
  }



  def createExpression(expression: String): Expression = {

    infixTokens = TOKENIZER.generateTokens(expression)
    tokens = convertToRPN(infixTokens)

    println(s"RPN: ${tokens.map(_.expr).mkString(" ")}")

    solve(tokens)
  }

  def serVariablesToExpr(expression: Expression): Expression = {

    var res = expression
    for (variable <- res.distinctVars) {
      if (!variable.equals("i")) {
        val value = vars.getOrElse(variable, throw new EvaluateException(s"Unknown variable '$variable'"))
        res = res.setVar(variable, value)
      }
    }
    res
  }


  def setFunction(expr: String): Expression = {

    if (expr.count(_ == '(') != 1 || expr.count(_ == ')') != 1)
      throw new ParseException(s"Function execution have invalid params: $expr")

    val funcName = expr.substring(0, expr.indexOf("(")).trim
    val func = funcs.getOrElse(funcName, throw new EvaluateException(s"Unknown function '$funcName'"))

    val argString = expr.substring(expr.indexOf("(") + 1, expr.lastIndexOf(")"))
    val arguments = TOKENIZER.generateTokens(argString).filter(_.tType != SPACE)

    setFuncParams(func, arguments)
  }

  def setFuncParams(func: Function, arguments: Array[Token]): Expression = {

    validateFuncParams(func.getName, arguments, func.numVars)

    val args: ArrayBuffer[Expression] = new ArrayBuffer[Expression]()
    var unary = false

    for (arg <- arguments if arg.tType != SEPARATOR) {
      arg.tType match {
        case REALNUMBER =>
          if (unary) args.append(RealNumber(arg.expr.toDouble).changeSign)
          else args.append(RealNumber(arg.expr.toDouble))
          unary = false
        case LITERAL if vars.contains(arg.expr) =>
          if (unary) args.append(vars(arg.expr).changeSign)
          else args.append(vars(arg.expr))
        case OPERATION => unary = true
        case _ => throw new EvaluateException(s"Wrong argument ${arg.expr} in function ${func.getName}")
      }
    }
    func(args.toArray:_*)
  }

  def validateFuncParams(funcName: String, arguments: Array[Token], numVars: Int): Unit = {

    if (numVars != arguments.count(x => x.tType != SEPARATOR && x.tType != OPERATION))
      throw new EvaluateException(
        s"""Wrong number of arguments in function '$funcName'.
           |Need $numVars, have ${arguments.count(_.tType != SEPARATOR)}""".stripMargin)

    var maySep = false
    for (arg <- arguments) {
      arg.tType match {
        case REALNUMBER | LITERAL if !maySep => maySep = true
        case OPERATION if arg.expr.equals("-") && !maySep => maySep = false
        case SEPARATOR if !maySep =>
            throw new ParseException(s"Wrong arguments in function '$funcName': ${arguments.mkString(" ")}")
        case SEPARATOR => maySep = false
        case _ => throw new EvaluateException(s"Wrong argument ${arg.expr} in function '$funcName''")
      }
    }
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
