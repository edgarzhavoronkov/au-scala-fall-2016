import scala.io.Source

/*
TODO Прочитайте содержимое данного файла.
В случае неудачи верните сообщение соответствующего исключения.
 */
def readThisWorksheet(): String = {
  try {
    Source
      .fromFile("D:/University/repos/au-scala-fall-2016/src/main/scala/hw1/HomeTask.sc", "UTF-8")
      .getLines
      .mkString("\n")
  } catch {
    case e: Exception => e.getMessage
  }
}

readThisWorksheet()
