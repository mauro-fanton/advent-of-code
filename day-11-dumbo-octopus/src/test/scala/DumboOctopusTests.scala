
import DumboOctopusApp.{getSynchronisedFlashing, measureEnergyLevel}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.equal
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class DumboOctopusTests extends AnyFlatSpec{


  val input =
    "5483143223\n" +
      "2745854711\n" +
      "5264556173\n" +
      "6141336146\n" +
      "6357385478\n" +
      "4167524645\n" +
      "2176841721\n" +
      "6882881134\n" +
      "4846848554\n" +
      "5283751526"
  val arrayInput = input.split('\n').flatMap(o => o.split("").map(_.toInt))
  val offset = input.indexOf('\n')

  "measureEnergyLevel" should "return the total flashing fish for 2 steps" in {
    val actual = measureEnergyLevel(arrayInput, offset, 2)
    actual should equal(35)
  }

  "measureEnergyLevel" should "return the total flashing fish for 10 steps" in {
    val actual = measureEnergyLevel(arrayInput, offset, 10)
    actual should equal(204)
  }

  "measureEnergyLevel" should "return the total flashing fish for 100 steps" in {
    val actual = measureEnergyLevel(arrayInput, offset, 100)
    actual should equal(1656)
  }

  "getSynchronisedFlashing" should "return the step at which the fish flash simultaneously" in {


    val actual = getSynchronisedFlashing(arrayInput, offset)
    actual should equal(195)
  }
}
