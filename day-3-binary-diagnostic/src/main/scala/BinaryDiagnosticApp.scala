import BinaryDiagnosticApp.{binaryToDecimal, filterBYMostCommonValue}

import scala.::
import scala.annotation.tailrec
import scala.io.Source

object BinaryDiagnosticApp extends App {
  def calculateLifeSupportRating(report: String): Int = {
    calculateOxygenGeneratorRating(report) * calculateCO2ScrubberRating(report)
  }

  def calculateCO2ScrubberRating(report: String): Int = {
    binaryToDecimal(
      filterBYLessCommonValue(report.split('\n'))(0),
      0,
      0)
  }

  def filterBYLessCommonValue(report: Array[String]): Array[String] = {
    filter(report, '0', (a, b) => a < b)
  }

  def calculateOxygenGeneratorRating(report: String): Int = {

    binaryToDecimal(
      filterBYMostCommonValue(report.split('\n'))(0),
      0,
      0)
  }

  def filterBYMostCommonValue(report: Array[String]): Array[String] = {
    filter(report, '1', (a, b) => a > b)
  }


  def binaryToDecimal(binary: String, acc: Int, expAcc: Int) : Int = {

    if(binary.isEmpty) return acc

      binary.takeRight(1) match {
        case "1" => binaryToDecimal(binary.dropRight(1), acc + Math.pow(2, expAcc).toInt, expAcc + 1)
        case _ =>  binaryToDecimal(binary.dropRight(1), acc, expAcc + 1)
      }
  }

  def calculatePowerConsumption(report: String): PowerConsumption = {

    val powerValue = getTheMostCommonBitInPosition(report.split('\n'))

    @tailrec
    def calculate(powerValue: String, gammaAcc: Int, epsilonAcc: Int, indexAcc: Int): PowerConsumption = {

      if(powerValue.isEmpty) return PowerConsumption(gammaAcc, epsilonAcc)

      val elem = powerValue.takeRight(1)

      if(elem == "0") calculate(powerValue.dropRight(1), gammaAcc, epsilonAcc + Math.pow(2, indexAcc).toInt, indexAcc + 1)
      else calculate(powerValue.dropRight(1), gammaAcc + Math.pow(2, indexAcc).toInt, epsilonAcc, indexAcc + 1)

    }

    calculate(powerValue, 0, 0, 0)
  }

  def filter(report: Array[String], bitToKeep: Char, fun: (Int, Int) => Boolean): Array[String] = {
      @tailrec
    def filterRecursive(powerValue: Array[String], acc: Array[String], accIndex: Int): Array[String] = {

      if(powerValue.isEmpty || powerValue.length == 1) return acc

      val bitesWithMostOne = powerValue.filter(p => p.charAt(accIndex) == '1')
      val bitesWithMostZero = powerValue.filter(p => p.charAt(accIndex) == '0')

      if(fun(bitesWithMostOne.length,bitesWithMostZero.length )) filterRecursive(bitesWithMostOne, bitesWithMostOne, accIndex + 1)
      else if (bitesWithMostOne.length == bitesWithMostZero.length) {
        val byte = (bitesWithMostOne ++ bitesWithMostZero).filter(o => o.charAt(accIndex) == bitToKeep)
        filterRecursive(byte, byte, accIndex + 1)
      }
      else filterRecursive(bitesWithMostZero, bitesWithMostZero, accIndex + 1)
    }

    filterRecursive(report, Array(), 0)
  }

  def getTheMostCommonBitInPosition(report: Array[String]): String = {
    val bites = groupByVerticalOrder(report, Array())

    bites.map(b => {
      if(b.count(_ == '1') > b.count(_ == '0') ) "1" else "0"
    } ).mkString

  }

  def groupByVerticalOrder(report: Array[String], acc: Array[String]): Array[String] = {

    if(report.isEmpty) return acc

    val elem = report.head.sliding(1).toArray

    if(acc.isEmpty) return groupByVerticalOrder(report.drop(1), elem)

    groupByVerticalOrder(report.drop(1), acc.zip(elem).map {case (v1,v2) => s"$v1$v2"})
  }

  def readDataFile(filename: String): String = {
    val filePath  = s"${System.getProperty("user.dir")}/src/main/resources/${filename}"
    Source.fromFile(filePath).getLines.mkString("\n")
  }

  val report = readDataFile("part-1-input.txt")
  val powerConsumption = calculatePowerConsumption(report)

  for (i <- 1 to 2) println("*********************************************************")

  println("\tDay 3 - BinaryDiagnostic part 1:")
  println(s"\tGamma is: ${powerConsumption.gamma}")
  println(s"\tEpsilon is: ${powerConsumption.epsilon}")
  println(s"\tPower Consupmtion is  = ${powerConsumption.calculate()}")

  println("\n*********************************************************\n")

  val oxygenGeneratorRating = calculateOxygenGeneratorRating(report)
  val CO2ScrubberRating = calculateCO2ScrubberRating(report)

  println("\tDay 3 - BinaryDiagnostic part 2:")
  println(s"\tOxygen Generator Rating: ${oxygenGeneratorRating}")
  println(s"\tCO2 Scrubber Rating: ${CO2ScrubberRating}")
  println(s"\tLife Support Rating: ${oxygenGeneratorRating * CO2ScrubberRating}")

  for (i <- 1 to 2) println("*********************************************************")
}
