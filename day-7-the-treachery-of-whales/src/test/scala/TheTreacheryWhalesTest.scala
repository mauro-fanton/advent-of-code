import TheTreacheryWhalesApp.{fuelConstantRateFun, fuelConsumption, fuelInconstantRateFun}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, equal}

class TheTreacheryWhalesTest extends AnyFlatSpec {

  val positions = "16,1,2,0,4,2,7,1,2,14".split(',').map(_.toInt)

  "calculateFuelConsumption" should "calculate fuel consumption for each move" in {

    val actual = fuelConsumption(positions, (pos1, pos2) => fuelConstantRateFun(pos1,pos2))

    actual(2) should equal(37)
    actual(1) should equal(41)
    actual(3) should equal(39)
    actual(10) should equal(71)
    actual.minBy(o => o._2)._2 should equal(37)
  }


  "fuelConsumptionAtInconstantRate" should "calculate fuel consumption at inconstant rate" in {

    val actual = fuelConsumption(positions, (pos1, pos2) => fuelInconstantRateFun(pos1,pos2))

    actual(5) should equal(168)
    actual(2) should equal(206)
    actual.minBy(o => o._2)._2 should equal(168)
  }
}
