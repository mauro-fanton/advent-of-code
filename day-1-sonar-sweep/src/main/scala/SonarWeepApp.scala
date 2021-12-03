import scala.io.Source

object SonarWeepApp extends App {

  def countNumberOfIncreases(numbers: String): Int = {
    findTotalIncreases(numbers.split("\n").map(s => s.toInt))
  }

  def findTotalIncreases(numArr: Array[Int]): Int = {
    var sum = 0;
    numArr.zipWithIndex.foreach { case (n, i) => if (i < numArr.length - 1 && n < numArr(i + 1)) {sum += 1}}

    sum
  }

  def findTotalIncreasesWithinASlidingWindows(numbers: String): Int = {

    val numbersInt = numbers.split('\n').map(o => o.toInt)
    val sumOfAllSlidingWindows = numbersInt.zipWithIndex.map { case (_, i) => numbersInt.slice(i, i + 3).toList.sum }

    findTotalIncreases(sumOfAllSlidingWindows.toArray)
  }

  def readDataFile(): String = {
    val filePath  = s"${System.getProperty("user.dir")}/src/main/resources/data.txt"
    Source.fromFile(filePath).getLines.mkString("\n")
  }


  val input = readDataFile()

  println("Day 1 - SonarWeepApp challenge part 1:")
  println(s"Number of Increase in Input: ${countNumberOfIncreases(input)}")
  println("\n*************************************************************\n")
  println("Day 1 - SonarWeepApp challenge part 2:")
  println(s"Number of sliding windows Increase in Input: ${findTotalIncreasesWithinASlidingWindows(input)}")

}
