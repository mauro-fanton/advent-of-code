import scala.io.Source

object DiveApp extends  App {

  def calculate(directions: Array[Direction], horPosAcc: Int, depthAcc: Int): (Int,Int) = {

    if(directions == Nil || directions.isEmpty)  return (horPosAcc, depthAcc)

    directions.head match {
      case Direction("forward", value) => calculate(directions.drop(1), horPosAcc + value, depthAcc)
      case Direction("down", value) => calculate(directions.drop(1), horPosAcc, depthAcc + value)
      case Direction("up", value) => calculate(directions.drop(1), horPosAcc, depthAcc - value)
    }
  }

  def calculateWithAim(directions: Array[Direction], horPosAcc: Int, depthAcc: Int, aimAcc: Int): (Int,Int) = {

    if(directions == Nil || directions.isEmpty)  return (horPosAcc, depthAcc)

    directions.head match {
      case Direction("forward", value) => calculateWithAim(directions.drop(1), horPosAcc + value, depthAcc + (value * aimAcc), aimAcc )
      case Direction("down", value) => calculateWithAim(directions.drop(1), horPosAcc, depthAcc, aimAcc + value)
      case Direction("up", value) => calculateWithAim(directions.drop(1), horPosAcc, depthAcc, aimAcc - value)
    }
  }

  def calculatePath(directions: String): (Int, Int) = {
    val array = parseDirection(directions)
    calculate(array, 0, 0)
  }

  def calculatePathWithAim(directions: String): (Int, Int) = {
    val array = parseDirection(directions)
    calculateWithAim(array, 0, 0, 0)
  }

  def parseDirection(instruction: String): Array[Direction] =
  {
    instruction.split('\n').map(o => {
      val arr = o.split(' ')
      Direction(arr(0), arr(1).toInt)
    })
  }

  def readDataFile(filename: String): String = {
    val filePath  = s"${System.getProperty("user.dir")}/src/main/resources/${filename}"
    Source.fromFile(filePath).getLines.mkString("\n")
  }

  val directions = calculatePath(readDataFile("input.txt"))

  for (i <- 1 to 2) println("*********************************************************")

  println("\tDay 2 - Dive challenge part 1:")
  println(s"\tHorizontal Position is: ${directions._1}")
  println(s"\tDepth Position is: ${directions._2}")
  println(s"\tHorizontal Position times Depth = ${directions._1 * directions._2}")

  println("\n*********************************************************\n")

  val directionsWithAim = calculatePathWithAim(readDataFile("part-2-input.txt"))

  println("\tDay 2 - Dive challenge part 2:")
  println(s"\tHorizontal Position is: ${directionsWithAim._1}")
  println(s"\tDepth Position is: ${directionsWithAim._2}")
  println(s"\tHorizontal Position times Depth = ${directionsWithAim._1 * directionsWithAim._2}")

  for (i <- 1 to 2) println("*********************************************************")
}
