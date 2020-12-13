package fr.fourtytwo.utils

import java.util.logging.{Formatter, LogRecord}


class ComputorFormatter extends Formatter {
  override def format(record: LogRecord): String = s"${record.getMessage}\n"
}
