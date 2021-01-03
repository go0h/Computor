package fr.fourtytwo

import fr.fourtytwo.computor.Computor

object Main {

  def main(args: Array[String]): Unit = {


    try {
      val computor = new Computor
      computor.run()
    } catch {
      case ex: Exception => println(s"${ex.getClass.getName} ${ex.getMessage}")
    }
  }

}