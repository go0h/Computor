package fr.fourtytwo.exception

class ParseException(message: String, position: Int = 0) extends Exception(message) {

  override def getMessage: String = {
    if (position == 0)
      return super.getMessage
    super.getMessage + printPoint
  }

  def printPoint: String = "\n" + "-" * (message.length - position) + "^"
}
