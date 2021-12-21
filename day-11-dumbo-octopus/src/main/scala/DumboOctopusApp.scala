import DumboOctopusApp.offset

import scala.annotation.tailrec

object DumboOctopusApp extends App {

  val inputGrid =
    "6636827465\n" +
    "6774248431\n" +
    "4227386366\n" +
    "7447452613\n" +
    "6223122545\n" +
    "2814388766\n" +
    "6615551144\n" +
    "4836235836\n" +
    "5334783256\n" +
    "4128344843"

  val arrayInput = inputGrid.split('\n').flatMap(o => o.split("").map(_.toInt))
  val offset = inputGrid.indexOf('\n')

  @tailrec
  final def increment(acc: Array[Int], pos: Array[Int]): Array[Int] = {

    if(pos.isEmpty) return acc

    val current = pos.head

    if(acc(current) == 0) increment(acc, pos.drop(1))
    else increment(acc.updated(current, acc(current) + 1), pos.drop(1))

  }

  def getNeighbours(cellIndex: Int, offset: Int, len: Int): Array[Int] = {

    val isLeftEdge = (cellIndex % offset) == 0

    val diagonalLeftUp = if(!isLeftEdge && cellIndex - offset - 1 >= 0) Array(cellIndex - offset - 1) else Array()
    val diagonalLeftDown = if(!isLeftEdge && cellIndex + offset - 1 < len) Array(cellIndex + offset - 1) else Array()
    val left1 = if(!isLeftEdge ) Array(cellIndex - 1) else Array()

    val isRightEdge = Math.ceil(cellIndex + 0.1) % offset == 0
    val diagonalRightUp = if(!isRightEdge && cellIndex - offset + 1 >= 0) Array(cellIndex - offset + 1) else Array()
    val diagonalRightDown = if(!isRightEdge && cellIndex + offset + 1 < len) Array(cellIndex + offset + 1) else Array()
    val right1 = if(!isRightEdge) Array(cellIndex + 1) else Array()

    val up = if(cellIndex - offset >= 0) Array(cellIndex - offset) else Array()
    val down = if(cellIndex + offset < len) Array(cellIndex + offset) else Array()

    left1 ++ right1 ++ diagonalLeftUp ++ diagonalLeftDown ++ diagonalRightUp ++ diagonalRightDown ++ up ++ down
  }

  @tailrec
  final def getFlashingOctopusCount(grid: Array[Int], offset: Int, indexes: Array[Int], countAcc: Int): (Int, Array[Int]) ={
    if(indexes.isEmpty) return (countAcc, grid)

    val current = indexes.head
    val adjacent = getNeighbours(current, offset, grid.length)
    val v = increment(grid, adjacent).updated(current, 0)

    val newIndexes = v.zipWithIndex.filter { case (o,i) => o > 9 }.map(_._2).sorted

    getFlashingOctopusCount(v, offset, newIndexes,  countAcc + 1)

  }
  def measureEnergyLevel(input: Array[Int], offset: Int, numOfSteps: Int): Int = {

    var scoreGridTuple = (0, input)

    for(step <- 0 until  numOfSteps) {
      val incrementedGrid = scoreGridTuple._2.map( o => o + 1)

      val flashingOctopus = incrementedGrid.zipWithIndex.filter { case (o, i) => o > 9 }.map(_._2)
      scoreGridTuple = getFlashingOctopusCount(incrementedGrid, offset, flashingOctopus, scoreGridTuple._1)
    }

    scoreGridTuple._1
  }

  def getSynchronisedFlashing(grid: Array[Int], offset: Int): Int = {


    @tailrec
    def loop (grid: Array[Int], offset: Int, accStep: Int): Int = {

      if(grid.filter( o => o != 0).isEmpty) return accStep

      val incrementedGrid = grid.map( o => o + 1)

      val flashingOctopus = incrementedGrid.zipWithIndex.filter { case (o, i) => o > 9 }.map(_._2)
      val scoreGridTuple = getFlashingOctopusCount(incrementedGrid, offset, flashingOctopus, 0)
      loop(scoreGridTuple._2, offset, accStep + 1)

    }

    loop(grid, offset, 0)
  }

  for (i <- 1 to 2) println("*********************************************************")

  println("\tDay 11 - Dumbo Octopus. Part 1:")
  println(s"\tTotal Flashes: ${measureEnergyLevel(arrayInput, offset, 100)}")


  println("\n*********************************************************\n")



  println("\tDay 14 - Extended Polymerization. Part 2:")
  println(s"\tStep at which all the fish flash simultaneously: ${getSynchronisedFlashing(arrayInput, offset)}")


  for (i <- 1 to 2) println("*********************************************************")
}
