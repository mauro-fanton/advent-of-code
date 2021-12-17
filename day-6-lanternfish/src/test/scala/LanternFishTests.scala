import LanternFishApp.{Counter, InternalTimer, calculateGrow, getGrows}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, equal}

class LanternFishTests extends AnyFlatSpec {

  val input = "3,4,3,1,2".split(',')


  val formattedInput: Array[(InternalTimer, Counter)] = (for {
    i <- input
    counter = input.count(_ ==  i)
  } yield(i.toInt, counter.toLong)).distinct


  "getGrows" should "return an array of tuple showing the internal time and a counter after one day" in {
   val actual = getGrows(formattedInput, 1)

    actual.length should equal(4)
    actual.sortBy(_._1) should equal(Array((2, 2), (3, 1), (0, 1),(1,1)).sortBy(_._1))
  }

  "getGrows" should "return an array of tuple showing the internal time and a counter after 2 day" in {
    val actual = getGrows(formattedInput, 2)

    actual.sortBy(_._1) should contain theSameElementsInOrderAs(Array((2, 1), (6, 1), (0, 1),(1,2), (8,1)).sortBy(_._1))
  }

  "getGrows" should "return an array of tuple showing the internal time and a counter after 3 day" in {
    val actual = getGrows(formattedInput, 3)

    actual.length should equal(6)
    actual.sortBy(_._1) should
      contain theSameElementsInOrderAs(Array((5, 1), (6, 1), (0, 2),(1,1), (8,1), (7,1)).sortBy(_._1))
  }

  "getGrows" should "return an array of tuple showing the internal time and a counter after 4 day" in {
    val actual = getGrows(formattedInput, 4)

    actual.sortBy(_._1) should
      contain theSameElementsInOrderAs(Array((6,3), (0,1),(4,1),(5,1),(7,1),(8,2)).sortBy(_._1))
  }

  "getGrows" should "return an array of tuple showing the internal time and a counter after 5 days" in {
    val actual = getGrows(formattedInput, 5)

    actual.sortBy(_._1) should
      contain theSameElementsInOrderAs(Array(
      (5,3),(6,2),(3,1),(4,1),(7,2),(8,1)).sortBy(_._1))
  }

  "getGrows" should "return an array of tuple showing the internal time and a counter after 6 days" in {
    val actual = getGrows(formattedInput, 6)

    actual.sortBy(_._1) should
      contain theSameElementsInOrderAs(Array(
      (4,3),(5,2),(2,1),(3,1),(6,2),(7,1)).sortBy(_._1))
  }

  "getGrows" should "return an array of tuple showing the internal time and a counter after 9 days" in {
    val actual = getGrows(formattedInput, 9)

    actual.sortBy(_._1) should
      contain theSameElementsInOrderAs(Array(
      (1,3),(2,2),(6,1),(0,1),(3,2),(4,1),(8,1)).sortBy(_._1))
  }

  "getGrows" should "return an array of tuple showing the internal time and a counter after 12 days" in {
    val actual = getGrows(formattedInput, 12)

    actual.sortBy(_._1) should
      contain theSameElementsInOrderAs(Array(
      (5,4),(6,3),(3,1),(4,1),(0,2),(1,1),(7,3),(8,2)).sortBy(_._1))
  }

  "getGrows" should "return an array of tuple showing the internal time and a counter after 13 days" in {
    val actual = getGrows(formattedInput, 13)

    actual.sortBy(_._1) should
      contain theSameElementsInOrderAs(Array(
      (4,4),(5,3),(2,1),(3,1),(6,5),(0,1),(7,2),(8,2)).sortBy(_._1))
  }

  "getGrows" should "return an array of tuple showing the internal time and a counter after 15 days" in {
    val actual = getGrows(formattedInput, 15)

    actual.sortBy(_._1) should
      contain theSameElementsInOrderAs(Array(
      (2,4),(3,3),(0,1),(1,1),(4,5),(5,3),(6,2),(7,1)).sortBy(_._1))
  }

  "getGrows" should "return an array of tuple showing the internal time and a counter after 18 days" in {
    val actual = getGrows(formattedInput, 18)

    actual.sortBy(_._1) should
      contain theSameElementsInOrderAs(Array(
      (6,5),(0,3),(4,2),(5,1),(1,5),(2,3),(3,2),(7,1),(8,4)).sortBy(_._1))
  }

  "calculateGrow" should "return the total number of fish spawned over 80 days" in{

    val actual = calculateGrow(getGrows(formattedInput, 80))
    actual should equal(5934)
  }

  "calculateGrow" should "return the total number of fish spawned over 256 days" in{

    val actual = calculateGrow(getGrows(formattedInput, 256))
    actual should equal(26984457539L)
  }
}
