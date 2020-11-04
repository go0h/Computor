package fr.fourtytwo.exception

class ParseException(message: String, position: Int = 0) extends Exception(message) {

  override def getMessage: String = {
    if (position == 0)
      super.getMessage
    super.getMessage + printPoint
  }

  def printPoint: String = {
    val sb = new StringBuilder("\n")
    for (_ <- 0 until message.length - position)
      sb.append("-")
    sb.append("^")
    sb.toString()
  }
}
