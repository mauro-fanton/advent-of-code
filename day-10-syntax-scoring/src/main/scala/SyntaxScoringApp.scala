import constants.constants.{chars, closingCharScore, closingCharSeq, openCharScore}

import scala.annotation.tailrec
import scala.io.Source

object SyntaxScoringApp extends App {

  def findCorruptedChunk(pattern: Array[String]): String = {

    @tailrec
    def loop(pattern: String, acc: String, illegalCharAcc: String): String = {
      if (pattern.isEmpty) return illegalCharAcc

      val char = pattern.head.toString

      if(chars.exists(_._2 == char) && acc.takeRight(1) == chars.filter(_._2 == char).keys.last ) loop(pattern.drop(1), acc.dropRight(1), illegalCharAcc)
      else if(closingCharSeq.contains(char)) loop("", acc, illegalCharAcc + char)
      else loop(pattern.drop(1), acc + char, illegalCharAcc)

    }

    var result: String = ""
    for(i <- 0 until pattern.length) {
      result += loop(pattern(i), "", "")
    }
    result
  }

  def findSequenceOfClosingCharacter(pattern: Array[String]): Array[String] = {

    @tailrec
    def loop(pattern: String, acc: String, closingChar: String): String = {
      if (pattern.isEmpty) return closingChar

      val char = pattern.head.toString

      if(isValidChunk(acc, char) ) loop(pattern.drop(1), acc.dropRight(1), closingChar.dropRight(1))
      else if(closingCharSeq.contains(char)) loop("", "", "")
      else loop(pattern.drop(1), acc + char, closingChar + chars(char))
    }

    (for {
      i <- 0 until pattern.length
      res = loop(pattern(i), "", "")
    } yield res.reverse).filter(o => !o.isEmpty).toArray
  }

  private def isValidChunk(acc: String, char: String) = {
    closingCharSeq.contains(char) && acc.takeRight(1) == chars.filter(_._2 == char).keys.last
  }


  def calculateTotalSyntaxScore(corruptedPatter: String): Int = {
    chars.map(o => (o._2, corruptedPatter.count(_.toString == o._2) * openCharScore(o._2)))
      .foldLeft(0){(acc, o) => acc + o._2}

  }

  def calculateClosingCharScore(closingCharacter: Array[String]): Array[(String,Long)]   = {

    @tailrec
    def loop(charSeq: String, acc: Long, totalScore: Long) : Long = {

      if(charSeq.isEmpty) return acc

      val current = charSeq.head.toString
      val score = 5 * totalScore + closingCharScore(current)
      loop(charSeq.drop(1), score, score)
    }

    for {
      e <- closingCharacter
      score = loop(e, 0, 0  )
    } yield(e, score)

  }

  def findMiddleScore(scores: Array[(String,Long)]): Long = {
    val middlePos = Math.floor(scores.length / 2).toInt
    val sortedValues = scores.map(o => o._2).sorted.distinct
    sortedValues(middlePos )
  }

  def readDataFile(filename: String): String = {
    val filePath  = s"${System.getProperty("user.dir")}/src/main/resources/${filename}"
    Source.fromFile(filePath).getLines.mkString("\n")
  }

  val input = readDataFile("inputData.txt").split('\n')


  for (i <- 1 to 2) println("*********************************************************")

  println("\tDay 10 - Syntax Scoring. Part 1:")
  println(s"\ttotal syntax error score: ${calculateTotalSyntaxScore(findCorruptedChunk(input))}")

  println("\n*********************************************************\n")
  println("\tDay 10 - Syntax Scoring. Part 2:")
  println(s"\tDay 14 - Extended Polymerization. Part 2: ${findMiddleScore(calculateClosingCharScore(findSequenceOfClosingCharacter(input)))}")


  for (i <- 1 to 2) println("*********************************************************")

}
