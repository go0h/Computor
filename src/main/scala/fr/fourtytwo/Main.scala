package fr.fourtytwo

import scala.sys.exit
import fr.fourtytwo.exception._
import fr.fourtytwo.polynomial.Polynomial


object Main {

  val USAGE: String =
  """Usage: ./ComputorV1 "expression" [-d]
    |   -h, --help			display this help and exit
    |   -d, --debug			debug mode""".stripMargin

  def main(args: Array[String]): Unit = {

    var debug: Boolean = false

    if (args.contains("-h") || args.contains("--help") || args.isEmpty || args.length > 2) {
      println(USAGE)
      exit(0)
    }
    else if (args.length == 2 && (args(1).equals("-d") || args(1).equals("--debug"))) {
      debug = true
    }
    else if (args.length == 2) {
      println(USAGE)
      exit(0)
    }
    val expr: String = args(0)

    try {
      val polynomial = Polynomial(expr, debug)
      val solution = polynomial.solve
      println(s"""The solutions is:
                  |$solution""".stripMargin)
    } catch {
      case ex: ParseException => println(ex.getMessage)
      case ex: EvaluateException => println(ex.getMessage)
      case ex: Exception => println(ex.getClass + " " + ex.getMessage)
    }
  }
}
