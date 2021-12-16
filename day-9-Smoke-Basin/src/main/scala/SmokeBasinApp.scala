import scala.annotation.tailrec
import scala.io.Source

object SmokeBasinApp extends App {

  def getLowPoint(number: Int, index: Int, str: String): Array[Boolean] = {

    if(index < 0) Array()
    else if(index >= str.length) Array()
    else Array(str.charAt(index).toString.toInt > number)

  }
  def findLowPoints(input: String, offset: Int): Array[Int] = {

    @tailrec
    def loop(input: String, offset: Int, acc: Array[Int], indexAcc: Int): Array[Int] = {

      if(indexAcc >= input.length) return acc

      val s = input.charAt(indexAcc).toString.toInt

      val isLowPoint = !(getLowPoint(s, indexAcc - 1, input) ++
        getLowPoint(s, indexAcc + 1, input) ++
        getLowPoint(s, indexAcc - offset, input) ++
        getLowPoint(s, indexAcc + offset, input)).contains(false)

      if(isLowPoint) loop(input, offset, acc ++ Array(s), indexAcc + 1)
      else loop(input, offset, acc, indexAcc + 1)
    }
    loop(input, offset, Array(), 0)
  }

  def findLowPointswithIndex(input: String, offset: Int): Array[(Int, Int)] = {

    @tailrec
    def loop(input: String, offset: Int, acc: Array[(Int, Int)], indexAcc: Int): Array[(Int, Int)] = {

      if(indexAcc >= input.length) return acc

      val s = input.charAt(indexAcc).toString.toInt

      val isLowPoint = !(getLowPoint(s, indexAcc - 1, input) ++
        getLowPoint(s, indexAcc + 1, input) ++
        getLowPoint(s, indexAcc - offset, input) ++
        getLowPoint(s, indexAcc + offset, input)).contains(false)

      if(isLowPoint) loop(input, offset, acc ++ Array((indexAcc,s)), indexAcc + 1)
      else loop(input, offset, acc, indexAcc + 1)
    }
    loop(input, offset, Array(), 0)
  }

  def findRiskLevel(str: String, offset: Int): Int = {
    findLowPoints(str, offset).fold(0){ (o, i) => o + i + 1}
  }

  def getBasin(lowPoint: Int, index: Int, str: String): Int = {

    if(index < 0 || index >= str.length) return -1

    val adjacentNum = str.charAt(index).toString.toInt
    if((adjacentNum - lowPoint).abs == 1 && adjacentNum != 9) adjacentNum else -1

  }

  @tailrec
  private def loop(input: String, points: Array[Int], offset: Int, acc: Array[Int]): Array[Int] = {

    if(points.isEmpty) return acc
    if(acc.contains(points.head)) return acc

    val indexAt = points.head
    val currentNum = input.charAt(indexAt).toString.toInt

    val left = getBasin(currentNum, indexAt - 1, input)
    val up = getBasin(currentNum, indexAt - offset, input)
    val right =getBasin(currentNum, indexAt + 1, input)
    val down = getBasin(currentNum, indexAt + offset, input)

    val leftNeighbours =  if( left != -1 && !acc.contains(indexAt - 1)) Array(indexAt - 1) else Array()
    val upNeighbours = if( up != -1 && !acc.contains(indexAt - offset))  Array(indexAt - offset) else Array()
    val rightNeighbours = if ( right != -1 && !acc.contains(indexAt + 1)) Array(indexAt + 1) else Array()
    val downNeighbours = if ( down != -1 && !acc.contains(indexAt + offset)) Array(indexAt + offset) else Array()

    val neighbours: Array[Int] = leftNeighbours ++ upNeighbours ++ rightNeighbours ++ downNeighbours

    loop(input, (points.drop(1) ++ neighbours).distinct, offset, acc ++ Array(indexAt))

  }

  def findBasinForEachLowPoint(input: String,  offset: Int): Array[Array[Int]] = {
    val t = findLowPointswithIndex(input, offset)
      t.map(o => loop(input, Array(o._1), offset, Array()))
      .map( o => o.map(o => input.charAt(o).toString.toInt))


  }

  def findTheLargestBasins(basins: Array[Array[Int]]): Int = {
    val c = basins.map(o => o.length)
    val s = c.sorted
    val b = basins.map(o => o.length).sorted
    val u = basins.map(o => o.length).sorted.takeRight(3)
    basins.map(o => o.length).sorted.takeRight(3).fold(1) { (a,b) => a * b}

  }

  def readDataFile(filename: String): String = {
    val filePath  = s"${System.getProperty("user.dir")}/src/main/resources/${filename}"
    Source.fromFile(filePath).getLines.mkString("\n")
  }

  val input = readDataFile("inputData.txt")
  for (i <- 1 to 2) println("*********************************************************")

  println("\tDay 9 - Smoke Basin. Part 1:")
  println(s"\tRisk Level is: ${findRiskLevel(input.replaceAll("\n", ""), input.indexOf('\n'))}")


  println("\n*********************************************************\n")

  println("\tDay 9 - Smoke Basin. Part 2:")

  val t = findTheLargestBasins(findBasinForEachLowPoint(input.replaceAll("\n", ""), input.indexOf('\n')) )
  println(s"\tLargest Basin is:: ${t}")

  for (i <- 1 to 2) println("*********************************************************")
}
