import SyntaxScoringApp._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class SyntaxScoringTest extends AnyFlatSpec {

  val input = "[({(<(())[]>[[{[]{<()<>>\n" +
    "[(()[<>])]({[<{<<[]>>(\n" +
    "(((({<>}<{<{<>}{[]{[]{}\n" +
    "{<[[]]>}<{[{[{[]{()[[[]\n" +
    "<{([{{}}[<[[[<>{}]]]>[]]"

  val arrayInput = input.split('\n')

  "findCorruptedChunk" should "find corrupted chunk" in {

    val pattern =
      "{([(<{}[<>[]}>{[]{[(<()>\n" +
        "[[<[([]))<([[{}[[()]]]\n" +
        "[{[{({}]{}}([{[{{{}}([]\n" +
        "[<(<(<(<{}))><([]([]()\n" +
        "<{([([[(<>()){}]>(<<{{"

    val actual = findCorruptedChunk(pattern.split('\n'))
    actual should equal("})])>")
    actual.count(_ == '}') should equal(1)
    actual.count(_ == ')') should equal(2)
    actual.count(_ == ']') should equal(1)
    actual.count(_ == '>') should equal(1)
  }

  "findCorruptedChunk" should "find corrupted chunk in bigger pattern" in {

    val pattern =
      "[({(<(())[]>[[{[]{<()<>>\n" +
        "[(()[<>])]({[<{<<[]>>(\n" +
        "{([(<{}[<>[]}>{[]{[(<()>\n" +
        "(((({<>}<{<{<>}{[]{[]{}\n" +
        "[[<[([]))<([[{}[[()]]]\n" +
        "[{[{({}]{}}([{[{{{}}([]\n" +
        "{<[[]]>}<{[{[{[]{()[[[]\n" +
        "[<(<(<(<{}))><([]([]()\n" +
        "<{([([[(<>()){}]>(<<{{\n" +
        "<{([{{}}[<[[[<>{}]]]>[]]\n"

    val actual = findCorruptedChunk(pattern.split('\n'))


    actual.count(_ == '}') * 1197 should equal(1197)
    actual.count(_ == ')') * 3 should equal(6)
    actual.count(_ == ']') * 57 should equal(57)
    actual.count(_ == '>') * 25137 should equal(25137)
    calculateTotalSyntaxScore(actual) should equal(26397)
  }

  "findSequenceOfClosingCharacter" should "return the sequence of closing character" in {

    val actual = findSequenceOfClosingCharacter(arrayInput)

    actual should contain theSameElementsAs(Array("}}]])})]", ")}>]})", "}}>}>))))", "]]}}]}]}>", "])}>"))
  }

  "findSequenceOfClosingCharacter1" should "discard invalid line" in {

    val closingCharWithInvalidLine = arrayInput ++ Array("{([(<{}[<>[]}>{[]{[(<()>")
    val actual = findSequenceOfClosingCharacter(closingCharWithInvalidLine)

    actual should contain theSameElementsAs(Array("}}]])})]", ")}>]})", "}}>}>))))", "]]}}]}]}>", "])}>"))

  }

  "calculateClosingCharScore" should "calculate the sore" in {

    val actual = calculateClosingCharScore(findSequenceOfClosingCharacter(arrayInput))

    actual should contain theSameElementsAs(Array(
      ("}}]])})]",288957),
      (")}>]})",5566),
      ("}}>}>))))", 1480781),
      ("]]}}]}]}>", 995444),
      ("])}>", 294)))
  }


  "findMiddleScore" should "find the middle score" in {

    val actual = findMiddleScore(Array(
      ("}}",288957),
      ("{({", 5566),
      ("}>)", 1480781),
      ("dsds", 995444),
      ("fdfd", 294)
    ))

    actual should equal(288957)

  }
}
