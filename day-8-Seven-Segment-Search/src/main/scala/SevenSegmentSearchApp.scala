import Types.types.{OutPutValue, SignalPattern}
import constants.DigitLengthMapper.numSegmentsMatch

import scala.annotation.tailrec
import scala.io.Source

object SevenSegmentSearchApp extends App {


  def parserInput(input: String): Array[(SignalPattern, OutPutValue)] = {

    input.split('\n')
      .map(o => o split('|'))
      .map( o => (o(0).trim.split("\\s+"), o(1).trim.split("\\s+")))
  }

  def findDigitInInput(input: Array[(SignalPattern, OutPutValue)], digit: Array[Int]): Array[String] = {

    val numOfSegments = digit.map(o => numSegmentsMatch(o).length)

    numOfSegments.flatMap(numLength => {
      input.flatMap(elem => elem._2.filter(o => o.length == numLength).flatMap(outputValue => {
        elem._1.filter( pattern => pattern.length == outputValue.length && pattern.sorted == outputValue.sorted)
      }))
    })
  }

  def readDataFile(filename: String): String = {
    val filePath  = s"${System.getProperty("user.dir")}/src/main/resources/${filename}"
    Source.fromFile(filePath).getLines.mkString("\n")
  }

  def decode(input: Array[(SignalPattern, OutPutValue)]): Array[String] = {

    @tailrec
    def loop(input: Array[(SignalPattern, OutPutValue)], acc: Array[String]): Array[String] = {

      if(input.isEmpty) return acc

      val head = input.head
      val pattern = head._1
      val outPutValue = head._2.map(o => o.sorted)

      val segments = findSegmentPatter(pattern)

      val t = for {
        e <- outPutValue
        n = segments.filter(o => o._2.sorted == e.sorted)
        v =  n.last._1.toString if(n != null && n.size > 0 )
      } yield v
     // val sum = pattern.filter( o =>  segments.values.filter(v => v.sorted == o.sorted))


      loop(input.drop(1), acc ++ Array(t.mkString))

    }

    loop(input, Array())

  }

  def findSegmentPatter(pattern: Array[String]): Map[Int, String] = {

    val segments = findSpecialNumber(pattern)

    val segmentWithLengthOf5 = pattern.filter(o => o.length == 5)
    val three = segmentWithLengthOf5.filter( o => (segments(1) diff o) == "" )
    val five = segmentWithLengthOf5.filter( o => ((segments(4) diff segments(1)) diff o ) == "" )
    val two = segmentWithLengthOf5.filter( o => (segments(4)  diff o ).length == 2 )

    val segmentWithLengthOf6 = pattern.filter(o => o.length == 6)
    val six = segmentWithLengthOf6.filter( o =>  (segments(1) diff (segments(4) diff o)).length == 1 )
    val nine = segmentWithLengthOf6.filter( o => (segments(4) diff o ).length == 0 )
    val zero = segmentWithLengthOf6.filter( o => ((segments(4) diff o) diff segments(1)).length == 1 )

    segments ++ Map(3 -> three(0), 5 -> five(0) , 2 -> two(0), 6 -> six(0), 9 -> nine(0), 0 -> zero(0))
  }

  def findSpecialNumber(pattern: Array[String]): Map[Int, String] = {

    @tailrec
    def loop(pattern: Array[String], acc: Map[Int, String]): Map[Int, String] = {

      if (pattern.isEmpty) return acc

      val str = pattern.head
      str.length match {
        case 2 => loop(pattern.drop(1), acc ++ Map(1 -> str))
        case 3 => loop(pattern.drop(1), acc ++ Map(7 -> str))
        case 4 => loop(pattern.drop(1), acc ++ Map(4 -> str))
        case 7 => loop(pattern.drop(1), acc ++ Map(8 -> str))
        case _ => loop(pattern.drop(1), acc)
      }
    }
    loop(pattern, Map())
  }


  val input = parserInput(readDataFile("inputData.txt"))

  for (i <- 1 to 2) println("*********************************************************")

  println("\tDay 8 - Seven Segment Search. Part 1:")
  println(s"\tOccurrence of digit in Oupt Put Value: ${findDigitInInput(input, Array(1,4,7,8)).length}")


  println("\n*********************************************************\n")

  println("\tDay 8 - Seven Segment Search. Part 2:")
  println(s"\tOccurrence of digit in Oupt Put Value: ${decode(input).map(_.toInt).sum}")

  println("\n*********************************************************\n")
}
