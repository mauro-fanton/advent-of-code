import ExtendedPolymerizationApp.{calculateTheLessCommonElement, calculateTheMOstCommonElement, insertRuleCounter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ExtendedPolymerizationTests extends AnyFlatSpec {

  val polymerTemplate = "NNCB"
  val pairInsertion = "CH -> B\n" +
    "HH -> N\n" +
    "CB -> H\n" +
    "NH -> C\n" +
    "HB -> C\n" +
    "HC -> B\n" +
    "HN -> C\n" +
    "NN -> C\n" +
    "BH -> H\n" +
    "NC -> B\n" +
    "NB -> B\n" +
    "BN -> B\n" +
    "BB -> N\n" +
    "BC -> B\n" +
    "CC -> N\n" +
    "CN -> C"


  val pairInsertionMap = pairInsertion.split('\n').map( o => {
    val arr = o.replaceAll(" ", "").split("->")
    (arr(0), arr(1))
  }).toMap

  "insertRule" should "insert rule with 1 step" in {
    val actual = insertRuleCounter(pairInsertionMap, "NNCB", 1)
      .toArray
      .map(o => (o._1.split("")(1),o._2))

    actual should contain theSameElementsInOrderAs(Array(("N" -> 1l), ("H", 1l), ("B",1l),("C",1l),("N",1l),("C",1l), ("B",1l)))
  }

 "calculateTheMostCommonElement" should "return the most common element" in {

   val actual = calculateTheMOstCommonElement(insertRuleCounter(pairInsertionMap, "NNCB", 10))
   actual should equal(("B", 1749l))
  }

  "calculateTheLessCommonElement" should "return the less common element" in {

    val actual = calculateTheLessCommonElement(insertRuleCounter(pairInsertionMap, "NNCB", 10))
    actual should equal("H", 161l)
  }

  "calculateDiff" should "return the diff for 10 steps" in {
    val insertionRule = insertRuleCounter(pairInsertionMap, "NNCB", 10)



    val mostCommonValue = calculateTheMOstCommonElement(insertionRule)
    val lessCommonValue = calculateTheLessCommonElement(insertionRule)

     (mostCommonValue._2 - lessCommonValue._2) should equal(1588)
  }

  "calculateDiff" should "return the diff for 40 steps" in {
    val insertionRule = insertRuleCounter(pairInsertionMap, "NNCB", 40)
    val mostCommonValue = calculateTheMOstCommonElement(insertionRule)
    val lessCommonValue = calculateTheLessCommonElement(insertionRule)

    (mostCommonValue._2 - lessCommonValue._2) should equal(2188189693529L)
  }


}
