import org.scalatest.flatspec.AnyFlatSpec

class SonarSweepTest extends AnyFlatSpec {

  val input = SonarWeepApp.readDataFile()

  "countNumberOfIncreases" should "count the number of increases" in {
    assertResult(1521) {SonarWeepApp.countNumberOfIncreases(input)}
  }

  "findTotalIncreasesWithinASlidingWindows" should "cound of number of increases for sliding windows" in {
    assertResult(1543) {SonarWeepApp.findTotalIncreasesWithinASlidingWindows(  input)}
  }

}
