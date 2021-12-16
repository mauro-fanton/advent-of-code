import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable
import scala.collection.parallel.mutable.ParArray
import scala.io.Source

object ExtendedPolymerizationApp extends App {

  def insertRule(polymerTemplate: Map[String,String], template: String, numOfSteps: Int): String = {

    @tailrec
    def loop(polymerTemplate: Map[String,String], acc: String, indexAcc: Long, inc: Int, stepNum: Int): String = {
      if(indexAcc <= 0) return acc

      println("###################")
      println(s"StepNum => ${stepNum}; INC => ${inc}; IndexAcc => ${indexAcc}")
      if(inc == Int.MaxValue) {

        println(s"1) StepNum => ${stepNum}; inc: ${inc}; index: ${indexAcc}")
        println(inc.toLong)
      }
      //print("-")
      val rule = acc.substring(inc, inc + 2)
     // print("#")
      loop(polymerTemplate, acc.patch(inc + 1, polymerTemplate(rule), 0), indexAcc - 1, inc + 2, stepNum)
    }
    var polymer = template
    for( i <- 1 to numOfSteps) {
      polymer = loop(polymerTemplate, polymer, polymer.length - 1, 0, i)
      print(s"${i}.")
    }

    polymer
  }

  def insertRuleCopy(polymerTemplate: Map[String,String], template: String, numOfSteps: Int): String = {

    @tailrec
    def loop(polymerTemplate: Map[String,String], acc: String, inc: Int, stepNum: Int): String = {
      if(inc + 2 > acc.length) return acc


      println("###################")
      println(s"StepNum => ${stepNum}; INC => ${inc}")
      if(inc == Int.MaxValue) {

        println(s"1) StepNum => ${stepNum}; inc: ${inc}")
        println(inc.toLong)
      }

      val rule = acc.substring(inc, inc + 2)

      loop(polymerTemplate, acc.patch(inc + 1, polymerTemplate(rule), 0), inc + 2, stepNum)
    }


    var polymer = template
    for( i <- 1 to numOfSteps) {
      //val t = polymer.split("(?<=\\G..)")
      polymer = loop(polymerTemplate, polymer, 0, i)
      print(s"${i}.")
    }

    polymer
  }


  def insertRuleCopy1(polymerTemplate: Map[String,String], template: String, numOfSteps: Int): String = {

    @tailrec
    def loop(polymerTemplate: Map[String,String], acc: ParArray[String], inc: Int, stepNum: Int): ParArray[String] = {
      if(inc + 2 > acc.length) return acc

      val rule = acc(inc) + acc(inc + 1)

      println(s"${stepNum}-")
      loop(polymerTemplate, acc.par.patch(inc + 1, Array(polymerTemplate(rule)), 0), inc + 2, stepNum)
    }


    var polymer = template.split("").par
    for( i <- 1 to numOfSteps) {
      // Split every two char. THe dot define the number of char to split into
      // val t = polymer.split("(?<=\\G..)")

      polymer = loop(polymerTemplate, polymer, 0, i)
      print(s"${i}.")
    }

    polymer.mkString
  }

  def insertRuleCounter(polymerTemplate: Map[String,String], template: String, numOfSteps: Int): Map[String, Long] = {

    @tailrec
    def loop(polymerTemplate: Map[String,String], template: Map[String, Long], acc: Map[String, Long],
             inc: Int): Map[String, Long] = {

      if(inc == template.size) return acc

      val currentKey = template.keysIterator.toList(inc)
      val rule = polymerTemplate(currentKey)

      val occurences = template(currentKey)
      val a = currentKey.split("")
      val insertionRules = Map(s"${a(0)}$rule" -> occurences, s"$rule${a(1)}" -> occurences)

      loop(polymerTemplate, template, acc ++ updateInsertionRules(acc, insertionRules), inc + 1)
    }

    var acc: Map[String, Long] = splitPolymerIntoPairInsertion(template)

    for( i <- 1 to numOfSteps) { acc = loop(polymerTemplate, acc, Map(), 0) }
    acc ++ Array((s"#${template.charAt(0).toString}" -> 1L))
  }

  def filterValues(insertionRuleCounter: Map[String, Long]): Array[(String, Long)] = {
    val v = insertionRuleCounter.toArray.map(o => (o._1.split("")(1),o._2))

    @tailrec
    def filter(arrayValue: Array[(String, Long)], acc: Array[(String, Long)]): Array[(String, Long)] = {

      if(arrayValue.isEmpty) return acc

      val value = arrayValue.head

      val found = acc.filter(o => o._1 == value._1)
      if(found.length == 0) {
        val sum = arrayValue.filter(o => o._1 == value._1).foldLeft(0L){(acc, b) => acc + b._2}
        filter(arrayValue.drop(1), acc ++ Array((value._1, sum)))
      }
      else filter(arrayValue.drop(1), acc)

    }

    filter(v, Array())
  }

  private def updateInsertionRules(acc: Map[String, Long], insertionRules: Map[String, Long]): Map[String, Long] = {
    for {
      (k, v) <- insertionRules
      newValue = if (acc.filter(o => o._1 == k).size > 0) acc(k) + v else v
    } yield (k, newValue)
  }

  def splitPolymerIntoPairInsertion(polymer: String): Map[String, Long] = {
    (for {
      i: Int <- 0 until polymer.length - 1
      c = if (i < polymer.length - 1) polymer.charAt(i).toString
      b = if (i < polymer.length - 1) polymer.charAt(i+1).toString
    } yield (s"$c$b" -> 1L)).toMap
  }

  def calculateTheMOstCommonElement(polymer: Map[String, Long]): (String, Long) = {
    filterValues(polymer).maxBy(_._2)
  }

  def calculateTheLessCommonElement(polymer: Map[String, Long]): (String, Long) = {
    filterValues(polymer).minBy(_._2)
  }

  def readDataFile(filename: String): String = {
    val filePath  = s"${System.getProperty("user.dir")}/src/main/resources/${filename}"
    Source.fromFile(filePath).getLines.mkString("\n")
  }

  val insertionRule = readDataFile("insertionRules.txt")
    .split('\n')
    .map( o =>
    {
    val arr = o.replaceAll(" ", "").split("->")
    (arr(0), arr(1))
    })
    .toMap
  val polymerTemplate = readDataFile("polymerTemplate.txt")

  val polymerWithRules= insertRuleCounter(insertionRule, polymerTemplate, 10)
  val mostCommonElement = calculateTheMOstCommonElement(polymerWithRules)._2;
  val lessCommonElement = calculateTheLessCommonElement(polymerWithRules)._2;

  for (i <- 1 to 2) println("*********************************************************")

  println("\tDay 14 - Extended Polymerization. Part 1:")
  println(s"\tMost common element subtract less common element diff: ${mostCommonElement - lessCommonElement}")


  println("\n*********************************************************\n")

  val polymerWithRulesFor40Iteration = insertRuleCounter(insertionRule, polymerTemplate, 40)
  val mostCommonElementFor40Iteration = calculateTheMOstCommonElement(polymerWithRulesFor40Iteration)._2;
  val lessCommonElementFor40Iteration = calculateTheLessCommonElement(polymerWithRulesFor40Iteration)._2;

  println("\tDay 14 - Extended Polymerization. Part 2:")
  println(s"\tMost common element subtract less common element diff: ${mostCommonElementFor40Iteration - lessCommonElementFor40Iteration}")

  for (i <- 1 to 2) println("*********************************************************")
}
