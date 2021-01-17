package fr.fourtytwo.computor

import scala.collection.mutable.{ArrayBuffer, Map => MMap, Stack => ScalaStack}
import scala.io.Source
import java.io.{BufferedReader, PrintStream}
import fr.fourtytwo.exception._
import fr.fourtytwo.expression.Operator._
import fr.fourtytwo.expression._
import fr.fourtytwo.expression.function._
import fr.fourtytwo.polynomial.Polynomial
import fr.fourtytwo.token.Token
import fr.fourtytwo.token.TokenType._


class Computor {

  var infixTokens: Array[Token] = _
  var tokens: Array[Token] = _

  val vars: MMap[String, Expression] = MMap[String, Expression]() ++ getPredefVariables
  val funcs: MMap[String, Function] = MMap[String, Function]() ++ getPredefFunctions

  /* for tests */
  var out: PrintStream = System.out

  val history: ArrayBuffer[String] = ArrayBuffer[String]()

  def this(infix: Array[Token]) = {
    this()
    infixTokens = infix
    tokens = convertToRPN(infixTokens)
  }

  override def toString: String = infixTokens.map(x => x.expr).mkString("")

  def run(): Unit = run(new BufferedReader(Source.stdin.reader()), System.out)

  def run(in: BufferedReader, output: PrintStream): Unit = {

    out = output
    var line = in.readLine()
    while (line != null) {

      try {
        if (line.isEmpty || line.startsWith("/")) {
          options(line)
        }
        else {
          Computor.basicCheck(line)
          line.replaceAll("//s+", " ") match {
            case VAR_ASSIGN_R(varName, _, expr) => variableAssignment(varName, expr)
            case VAR_COMP_R(varName, _) => variableComputation(varName)
            case FUNC_ASSIGN_R(func, _, expr) => functionAssignment(func, expr)
            case FUNC_COMP_R(func, _) => functionComputation(func)
            case POLY_FUNC_COMP_R(func, _, expr) => polynomialComputation(func, expr)
            case COMMON_COMP(expr, _) => commonComputation(expr)
            case _ => throw new ParseException(s"Can't recognize expression type $line")
          }
        }
      } catch {
        case ex: ParseException => out.println(ex.getMessage)
        case ex: EvaluateException => out.println(ex.getMessage)
        case ex: ArithmeticException => out.println(ex.getMessage)
        case ex: Exception => out.println(s"${ex.getClass.getName} ${ex.getMessage}")
      }
      line = in.readLine()
      if (history.length > 5)
        history.remove(0)
    }
  }

  def solve: Expression = {
    if (tokens == null)
      throw new EvaluateException("Expression is not defined")
    solve(tokens)
  }

  def solve(tokens: Array[Token]): Expression = {

    var i = 0
    val stack = ScalaStack[Expression]()

    while (i < tokens.length) {

      tokens(i).tType match {
        case REALNUMBER => stack.push(RealNumber(tokens(i).expr.toDouble))
        case MATRIX => stack.push(Matrix(tokens(i).expr))
        case LITERAL => stack.push(Variable(tokens(i).expr))
        case FUNCTION => stack.push(setFunction(tokens(i).expr))
        case OPERATION =>
            if (stack.length < 2)
              throw new EvaluateException(s"Wrong expression: $toString")
            val first = stack.pop()
            stack.push(Operator(stack.pop(), tokens(i).expr, first).evaluate)
        case UNARY => {
          if (stack.isEmpty)
            throw new EvaluateException(s"Wrong expression: $toString")
          stack.push(stack.pop().changeSign)
        }
        case _ => throw new EvaluateException(s"Can't solve token: ${tokens(i).expr}")
      }
      i += 1
    }
    if (stack.length != 1)
      throw new EvaluateException(s"Wrong expression: $toString")
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

    if (varName.trim.equalsIgnoreCase("i"))
      throw new EvaluateException(s"Can't assign variable 'i', because it reserved for ComplexNumber")

    var res = createExpression(expression)
    if (res.countVars != 0) {
      res = serVariablesToExpr(res, res.distinctVars.toArray)
    }

    res = res.evaluate
    vars(varName.trim.toLowerCase) = res

    out.println(res)
    history.append(s"${varName.trim} = ${expression.trim}")
  }


  ////////////////////////////////////////
  ///// COMMON COMPUTATION METHODS ///////
  ////////////////////////////////////////
  def variableComputation(varName: String): Unit = {
    val name = varName.trim.toLowerCase
    val res = vars.getOrElse(name, throw new EvaluateException(s"Unknown variable '${varName.trim}'"))
    out.println(res)
    history.append(s"${varName.trim} = ?")
  }

  def functionAssignment(funcWithParams: String, expression: String): Unit = {

    var expr = createExpression(expression)

    val funcName = funcWithParams.substring(0, funcWithParams.indexOf("(")).trim

    val argString = funcWithParams
      .substring(funcWithParams.indexOf("(") + 1, funcWithParams.lastIndexOf(")"))

    val arguments = TOKENIZER.generateTokens(argString).filter(_.tType != SPACE)

    val funcArgs = arguments.filter(x => x.tType != SEPARATOR)
      .filter(!_.expr.equalsIgnoreCase("i"))
      .map(_.expr).distinct

    expr = serVariablesToExpr(expr, (expr.distinctVars -- funcArgs).toArray)

    validateFuncParams(funcName, arguments, expr.countVars)

    if ((expr.distinctVars -- funcArgs.map(_.toLowerCase)).nonEmpty)
      throw new ParseException(s"Function '$funcName' expected params '${funcArgs.mkString(", ")}', " +
        s"but have '${expr.distinctVars.mkString(", ")}'")

    val func = UserDefinedFunction(funcName, funcArgs, expr)

    funcs(funcName.toLowerCase) = func

    out.println(func.getExpr)
    history.append(s"${funcWithParams.trim} = ${expression.trim}")
  }

  def functionComputation(expr: String): Unit = {

    val funcName = expr.substring(0, expr.indexOf("(")).trim
    val func = funcs.getOrElse(funcName.toLowerCase, throw new EvaluateException(s"Unknown function '$funcName'"))

    val argString = expr.substring(expr.indexOf("(") + 1, expr.lastIndexOf(")"))
    val arguments = TOKENIZER.generateTokens(argString).filter(_.tType != SPACE)

    validateFuncParams(funcName, arguments, func.numVars)

    history.append(s"${expr.trim} = ?")

    for (arg <- arguments) {
      if (arg.expr.equalsIgnoreCase("i"))
        throw new EvaluateException(s"You can't pass variable 'i' into function, " +
          "because it reserved for ComplexNumber")

      if (arg.tType == LITERAL && !vars.contains(arg.expr)
        && func.distinctVars.contains(arg.expr)) {
        out.println(func)
        return
      }
    }
    out.println(setFunction(expr).evaluate)
  }

  def polynomialComputation(funcWithParams: String, expr: String): Unit = {

    val y = TOKENIZER.generateTokens(expr.replaceAll("\\?", "").trim)
    if (y.length != 1)
      throw new ParseException(s"Polynomial must equal only one variable")

    val right = y.head.tType match {
      case REALNUMBER => RealNumber(y.head.expr.toDouble)
      case LITERAL => vars.getOrElse(y.head.expr, throw new EvaluateException(s"Unknown function '${y.head.expr}'"))
      case _ => throw new EvaluateException(s"Invalid token type in polynomial equality: ${y.head.expr}")
    }

    val funcName = funcWithParams.substring(0, funcWithParams.indexOf("(")).trim

    val func = funcs.getOrElse(funcName.toLowerCase, throw new EvaluateException(s"Unknown function '$funcName'"))
    if (!func.isInstanceOf[UserDefinedFunction])
      throw new EvaluateException(s"Function '${func.getName}' is not UserDefinedFunction type")

    val funcUDF = func.asInstanceOf[UserDefinedFunction]

    val argString = funcWithParams
      .substring(funcWithParams.indexOf("(") + 1, funcWithParams.lastIndexOf(")"))
    val funcArgs = TOKENIZER.generateTokens(argString)
      .filter(x => x.tType != SPACE && x.tType != SEPARATOR)
      .map(_.expr.toLowerCase)

    if (funcArgs.length != 1)
      throw new ParseException(s"Polynomial function must have only one argument, but have ${funcArgs.length}")

    val funcVars = funcUDF.distinctVars
    if ((funcVars -- funcArgs).nonEmpty) {
      throw new ParseException(s"Function '$funcName' expected params '${funcArgs.mkString(", ")}', " +
        s"but have '${funcVars.mkString(", ")}'")
    }

    val newExpr = s"${funcUDF.getExpr.toStringWithOrder} + ${right.changeSign} = 0"

    out.println(Polynomial(newExpr).solve)
    history.append(s"${funcWithParams.trim} = ${expr.trim}")
  }

  def commonComputation(expression: String): Unit = {

    var res = createExpression(expression)

    if (res.countVars != 0) {
      res = serVariablesToExpr(res, res.distinctVars.toArray)
    }

    out.println(res.evaluate)
    history.append(s"${expression.trim} = ?")
  }


  ////////////////////////////////////////
  ///////// AUXILIARY METHODS ////////////
  ////////////////////////////////////////
  private def createExpression(expression: String): Expression = {

    infixTokens = TOKENIZER.generateTokens(expression)
    tokens = convertToRPN(infixTokens)

    solve(tokens)
  }

  private def serVariablesToExpr(expression: Expression, varNames: Array[String]): Expression = {

    var res = expression

    for (variable <- varNames) {
      if (!variable.equals("i")) {
        val value = vars.getOrElse(variable, throw new EvaluateException(s"Unknown variable '$variable'"))
        res = res.setVar(variable, value)
      }
    }
    res
  }

  private def setFunction(expr: String): Expression = {

    val funcName = expr.substring(0, expr.indexOf("(")).trim
    val func = funcs.getOrElse(funcName.toLowerCase, throw new EvaluateException(s"Unknown function '$funcName'"))

    val argString = expr.substring(expr.indexOf("(") + 1, expr.lastIndexOf(")"))
    val arguments = TOKENIZER.generateTokens(argString).filter(_.tType != SPACE)

    setFuncParams(func, arguments)
  }

  private def setFuncParams(func: Function, arguments: Array[Token]): Expression = {

    validateFuncParams(func.getName, arguments, func.numVars)

    val args: ArrayBuffer[Expression] = new ArrayBuffer[Expression]()
    var unary = false

    for (arg <- arguments if arg.tType != SEPARATOR) {
      arg.tType match {
        case REALNUMBER =>
          if (unary) args.append(RealNumber(arg.expr.toDouble).changeSign)
          else args.append(RealNumber(arg.expr.toDouble))
          unary = false
        case LITERAL if vars.contains(arg.expr.toLowerCase) =>
          if (unary) args.append(vars(arg.expr.toLowerCase).changeSign)
          else args.append(vars(arg.expr.toLowerCase))
        case OPERATION => unary = true
        case _ => throw new EvaluateException(s"Wrong argument '${arg.expr}' in function '${func.getName}'")
      }
    }
    func(args.toSeq:_*)
  }

  private def validateFuncParams(funcName: String, args: Array[Token], numVars: Int): Unit = {

    if (numVars != args.count(x => x.tType != SEPARATOR && x.tType != OPERATION))
      throw new EvaluateException(
        s"""Wrong number of arguments in function '$funcName'.
           |Need $numVars, have ${args.count(_.tType != SEPARATOR)}""".stripMargin)

    var maySep = false
    for (arg <- args) {
      arg.tType match {
        case REALNUMBER | LITERAL if !maySep => maySep = true
        case OPERATION if arg.expr.equals("-") && !maySep => maySep = false
        case SEPARATOR if !maySep =>
            throw new ParseException(s"Wrong arguments in function '$funcName': ${args.mkString(" ")}")
        case SEPARATOR => maySep = false
        case _ => throw new EvaluateException(s"Wrong argument ${arg.expr} in function '$funcName''")
      }
    }
  }


  private def options(line: String): Unit = {

    line match {
      case "" => println()
      case "/q" | "/quit" => System.exit(0)
      case "/h" | "/help" => println(HELP)
      case "/hist" |"/history" => println(history.mkString("\n"))
      case "/v" | "/vars" => println(vars.map(x => s"${x._1}: ${x._2.getType} = ${x._2}").mkString("\n"))
      case "/f" | "/funcs" => println(funcs.map(x => s"${x._2}").mkString("\n"))
      case _ => throw new ParseException(s"Can't recognize command $line")
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

  def main(args: Array[String]): Unit = {

    if (args.nonEmpty) {
      println("Usage ./ComputorV2.jar")
      System.exit(0)
    }

    val computor = new Computor
    computor.run()
  }
}
