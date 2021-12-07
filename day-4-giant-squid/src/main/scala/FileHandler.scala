import scala.io.Source

class FileHandler {

  def readDataFile(filename: String): String = {
    val filePath  = s"${System.getProperty("user.dir")}/src/main/resources/${filename}"
    Source.fromFile(filePath).getLines.mkString("\n")
  }
}

object FileHandler {
  def apply() = {
    new FileHandler()
  }
}
