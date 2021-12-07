import org.scalatest.flatspec.AnyFlatSpec

class BinaryDiagnosticTests extends AnyFlatSpec {


  "getTheMostCommonBit" should "return the most common Bit" in {
    val report = Array("00011", "11011", "00110")
    assertResult("00011") {
      BinaryDiagnosticApp.getTheMostCommonBitInPosition(report)
    }
  }

  "groupByVerticalOrder" should "return a list of bite grouped vertically" in {

    val report = Array("00011", "11011", "00110")
    assertResult(Array("010", "010", "001", "111", "110")) {
      BinaryDiagnosticApp.groupByVerticalOrder(report, Array())
    }
  }

  "calculatePowerConsumption" should "return the power consumption fgg   " in {
    val report = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

    val powerConsumption = BinaryDiagnosticApp.calculatePowerConsumption(report)
    assertResult(22) {
      powerConsumption.gamma
    }
    assertResult(9) {
      powerConsumption.epsilon
    }
  }


  "filterBYMostCommonValue" should "return the bite with more zero starting from most r" in {
    val report = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

    assertResult(Array("10111")) {
      BinaryDiagnosticApp.filterBYMostCommonValue(report.split('\n'))
    }
  }

  "filterBYLessCommonValue" should "return the bite with more zero starting from most r" in {
    val report = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

    /*
      00100, 01111,01111, 00010, 11010
     */
    assertResult(Array("01010")) {
      BinaryDiagnosticApp.filterBYLessCommonValue(report.split('\n'))
    }
  }

  "calculateOxygenGeneratorRating" should "return the oxygen generator reating" in {
    val report = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

    assertResult(23) {
      BinaryDiagnosticApp.calculateOxygenGeneratorRating(report)
    }
  }

  "calculateCO2ScrubberRating" should "return the CO2 Scrubber Rating" in {
    val report = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

    assertResult(10) {
      BinaryDiagnosticApp.calculateCO2ScrubberRating(report)
    }
  }

  "calculateLifeSupportRating" should "return Life support rating Rating" in {
    val report = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"

    assertResult(230) {
      BinaryDiagnosticApp.calculateLifeSupportRating(report)
    }
  }

}
