import scala.annotation.tailrec
import scala.io.Source

object LanternFishApp extends App {

  type InternalTimer = Int
  type Counter = Long

  def getGrows(input: Array[(InternalTimer, Counter)], days: Int): Array[(InternalTimer, Counter)] = {

    @tailrec
    def loop(input: Array[(InternalTimer, Counter)], index: Int, acc: Array[(InternalTimer, Counter)]): Array[(InternalTimer, Counter)] = {

      if(index < 0) return acc

      val value = input(index)

      val nextInternalTimer: InternalTimer = if(value._1 == 0) 6 else (value._1 - 1)

      val counter =
        if(value._1 == 0) updateCounter(acc, (6, value._2))
        else updateCounter(acc, (value._1 - 1, value._2))

      if(value._1 == 0) loop(input, index -1, acc.filter( o => o._1 != nextInternalTimer) ++ Array((6, counter), (8, value._2)))
      else if(acc.filter( o => o._1 == nextInternalTimer).length > 0) loop(input, index - 1, acc.filter( o => o._1 != nextInternalTimer) ++ Array((nextInternalTimer, counter)))
      else loop(input, index - 1, acc ++ Array((nextInternalTimer, counter)))

    }

    var result: Array[(InternalTimer, Counter)] = input
    for( i <- 0 until days) {
      result = loop(result, result.length - 1, Array())
    }
    result
  }

  def updateCounter(list: Array[(InternalTimer, Counter)], value: (InternalTimer, Counter)):  Counter = {

    val existing = list.filter(o => o._1 == value._1)
    if(!existing.isEmpty) return existing.foldLeft[Long](0) { (acc, b) => acc + b._2} + value._2
    else value._2
  }

  def calculateGrow(tuples: Array[(InternalTimer, Counter)]): Counter = {
    tuples.foldLeft[Long](0) {(acc,o) => acc + o._2}
  }

  def readDataFile(filename: String): String = {
    val filePath  = s"${System.getProperty("user.dir")}/src/main/resources/${filename}"
    Source.fromFile(filePath).getLines.mkString("\n")
  }


  val input = readDataFile("inputData.txt").split(',')


  val formattedInput: Array[(InternalTimer, Counter)] = (for {
    i <- input
    counter = input.count(_ ==  i)
  } yield(i.toInt, counter.toLong)).distinct
  for (i <- 1 to 2) println("*********************************************************")

  println("\tDay 6 - Lantern Fish. Part 1:")
  println(s"\tNumber of fish spawned after 80 days: ${calculateGrow(getGrows(formattedInput, 80))}")


  println("\n*********************************************************\n")

  println("\tDay 6 - Extended Polymerization. Part 2:")
  println(s"\tNumber of fish spawned after 256: ${calculateGrow(getGrows(formattedInput, 256))}")

  for (i <- 1 to 2) println("*********************************************************")
}
