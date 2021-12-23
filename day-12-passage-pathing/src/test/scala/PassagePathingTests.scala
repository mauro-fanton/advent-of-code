import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.annotation.tailrec

class PassagePathingTests extends AnyFlatSpec{

  val input = "dc-end\n" +
    "HN-start\n" +
    "start-kj\n" +
    "dc-start\n" +
    "dc-HN\n" +
    "LN-dc\n" +
    "HN-end\n" +
    "kj-sa\n" +
    "kj-HN\n" +
    "kj-dc"

  val arrayInput = input.split('\n')

  val startingInput = arrayInput.filter(o => o.contains("start"))
  val paths = arrayInput.filter(o => !o.contains("start"))


  def filterTheEndPath(paths: Array[String]): Array[String] = paths.filter(o => o.contains("end"))


  @tailrec
  final def findAPath(start: Array[String], paths: Array[String], acc: Array[String]): Array[String] = {

    if(start.filter(o => o != "end").length == 0) return acc

    val head = start.head
    val lastPath = head.substring(head.lastIndexOf(",") + 1, head.length)

    val completedPath = filterTheEndPath(start)
    if(completedPath.length > 0) findAPath(start.filter(o => !o.contains("end")).distinct, paths, (acc ++ completedPath).distinct)
    else {
      val possiblePath = paths.filter(o => o.contains(lastPath))
        .flatMap(o => o.split('-'))
        .filter(o => o != lastPath)

      val newPath = possiblePath.map(o => {

        if(o.toLowerCase() == o && head.contains(o.toLowerCase())) head
        else if(o.toUpperCase() == o && head.split(',').last == o.toUpperCase()) head
        else head + "," + o
      }).filter(o => o != head)


     findAPath((start.drop(1) ++ newPath).distinct, paths, acc)
    }

  }

  def findAllPath(startingPoints: Array[String], paths: Array[String]): Array[String] = {

    val parsedInput = startingPoints.filter(o => o.contains("start")).map(o => {
      val arr = o.split("-")
      if(arr(0) == "start") arr(0) + "," + arr(1) else arr(1) + "," + arr(0)
    })

    parsedInput.flatMap( o => findAPath(Array(o), paths,Array()))
  }


  "findAPath" should "should find a path" in {

    val expected = Array("start,HN,dc,HN,end",
      "start,HN,dc,HN,kj,HN,end",
      "start,HN,dc,end",
      "start,HN,dc,kj,HN,end",
      "start,HN,end",
      "start,HN,kj,HN,dc,HN,end",
      "start,HN,kj,HN,dc,end",
      "start,HN,kj,HN,end",
      "start,HN,kj,dc,HN,end",
      "start,HN,kj,dc,end")

    val actual = findAPath(Array("start,HN"), paths, Array())
    actual should contain theSameElementsAs(expected)
  }

  "findAPath" should "should find all path" in {

    val expected = Array("start,HN,dc,HN,end",
      "start,HN,dc,HN,kj,HN,end",
      "start,HN,dc,end",
      "start,HN,dc,kj,HN,end",
      "start,HN,end",
      "start,HN,kj,HN,dc,HN,end",
      "start,HN,kj,HN,dc,end",
      "start,HN,kj,HN,end",
      "start,HN,kj,dc,HN,end",
      "start,HN,kj,dc,end",
      "start,dc,HN,end",
      "start,dc,HN,kj,HN,end",
      "start,dc,end",
      "start,dc,kj,HN,end",
      "start,kj,HN,dc,HN,end",
      "start,kj,HN,dc,end",
      "start,kj,HN,end",
      "start,kj,dc,HN,end",
      "start,kj,dc,end")

    val actual = findAllPath(arrayInput, paths)
    actual should contain theSameElementsAs(expected)
  }
}
