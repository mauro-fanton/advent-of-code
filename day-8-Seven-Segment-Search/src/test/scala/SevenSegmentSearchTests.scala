import SevenSegmentSearchApp.{decode, findDigitInInput, findSegmentPatter, parserInput}
import org.scalatest.flatspec.AnyFlatSpec

class SevenSegmentSearchTests extends AnyFlatSpec {


  val input = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n" +
              "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n" +
              "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"

  val input2 = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n" +
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n" +
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n" +
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n" +
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n" +
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n" +
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n" +
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n" +
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n" +
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n"


  "findDigitInInput" should "find the 1, 4, 7, 8 in the signal pattern" in {

    val actual = findDigitInInput(parserInput(input), Array(1, 4, 7, 8))

    assert(Array("gcbe".sorted, "fdgacbe".sorted, "dgebacf".sorted, "cg", "cbg".sorted, "cg", "cg", "cbg".sorted).sorted
      sameElements actual.map(o => o.sorted).sorted)
  }

  "findDigitOccurrenceInInput" should "find the 1, 4, 7, 8 in the signal pattern" in {
    val actual = findDigitInInput(parserInput(input2), Array(1, 4, 7, 8))

    assertResult(26) {actual.length}
  }

  "decode" should "find all the segment pattern" in {

    val pattern = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

    assertResult(Map(
      0 -> "cagedb", 1 -> "ab", 8 -> "acedgfb", 4 -> "eafb",
      7 -> "dab", 3 -> "fbcad", 5 -> "cdfbe", 2 -> "gcdfa", 6 -> "cdfgeb", 9 -> "cefabd")) {findSegmentPatter(pattern.split("\\s+"))}
  }

  "decode" should "decode the output value" in {

    val pattern = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"

    val actual = decode(parserInput(input2))
    assertResult(Array("8394", "9781", "1197", "9361", "4873", "8418", "4548", "1625", "8717","4315" )) {actual}

    assertResult(61229) {actual.map(_.toInt).sum}
  }
}
