package fr.fourtytwo.utils

import java.util.logging.{ConsoleHandler, Level, Logger}


object ComputorLogger {

  private val handler = new ConsoleHandler
  handler.setFormatter(new ComputorFormatter)
  handler.setLevel(Level.ALL)

  val LOGGER: Logger = Logger.getLogger(getClass.getSimpleName)
  LOGGER.setUseParentHandlers(false)
  LOGGER.addHandler(handler)

}
