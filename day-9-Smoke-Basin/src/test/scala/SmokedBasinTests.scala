import SmokeBasinApp.{findBasinForEachLowPoint, findLowPoints, findRiskLevel, findTheLargestBasins}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import matchers.should.Matchers._

class SmokedBasinTests extends AnyFlatSpec {

  val input1 = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
  val cleanInput = input1.replaceAll("\n", "")
  val offset = input1.split('\n')(0).length


  "findLowPoints" should "find all low point in input" in {

    assertResult(Array(1, 0, 5, 5)) {findLowPoints(cleanInput, offset)}
  }

  "findRiskLevel" should "find the riskLevel" in {
    assertResult(15) {findRiskLevel(cleanInput, offset)}
  }

  "findBasinForEachLowPoint" should "return the index of number in basin" in {

    val actual = findBasinForEachLowPoint(cleanInput, offset)

    actual should equal(Array(
      Array(1, 2, 3),
      Array(0,1,2,3,4,4,2,1,2).sorted,
      Array(8,7,8,8,5,6,7,8,8,7,6,7,8,8).sorted,
      Array(8,6,7,8,6,5,6,7,8).sorted
    ))
  }

  "findTheLargestBasins" should "return the largest basin" in {

    val actual = findTheLargestBasins(findBasinForEachLowPoint(cleanInput, offset) )

    actual should equal(1134)
  }
}
