import scala.io.Source

object TheTreacheryWhalesApp extends App {

  def fuelConsumption(positions: Array[Int], f: (Int,Int) => Int): Map[Int,Int] = {

    val v = (for {
      pos <- 1 to positions.length
      i <- positions
      fuel = f(i,pos)
    } yield(pos, fuel))
      .toArray
      .groupBy(_._1)

      v.map(o => (o._1 -> o._2.foldLeft(0){(acc, v) => acc + v._2}))
  }

  def minFuelConsumption(fuelConsumption: Map[Int,Int]): Int = {
    fuelConsumption.minBy(o => o._2)._2
  }

  def fuelConstantRateFun(pos1: Int, pos2: Int): Int = {
    (pos1 - pos2).abs
  }
  def fuelInconstantRateFun(pos1: Int, pos2: Int): Int = {
    (1 to (pos1 - pos2).abs).toArray.fold(0){(a,b) => a + b}
  }

  def readDataFile(filename: String): String = {
    val filePath  = s"${System.getProperty("user.dir")}/src/main/resources/${filename}"
    Source.fromFile(filePath).getLines.mkString("\n")
  }

  val input = readDataFile("inputData.txt").split(',').map(_.toInt)
  for (i <- 1 to 2) println("*********************************************************")

  println("\tDay 14 - The Treachery of Whales. Part 1:")
  println(s"\tMinimum Fuel Consumption at constant Rate: ${minFuelConsumption(fuelConsumption(input, (pos1, pos2) => fuelConstantRateFun(pos1,pos2)))}")

  println("\n*********************************************************\n")

  println("\tDay 14 - Extended Polymerization. Part 2:")
  println(s"\tMinimum Fuel Consumption at inconstant Rate: ${minFuelConsumption(fuelConsumption(input, (pos1, pos2) => fuelInconstantRateFun(pos1,pos2)))}")

  for (i <- 1 to 2) println("*********************************************************")
}
