import org.scalatest.flatspec.AnyFlatSpec

class DiveAppTest extends AnyFlatSpec {

  val directions = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

  "calculatePath" should "return and array of instruction" in {

    val expectedArray = Array(
      Direction("forward", 5),
      Direction("down", 5),
      Direction("forward", 8),
      Direction("up", 3),
      Direction("down", 8),
      Direction("forward", 2)
    )
    assertResult(expectedArray) { DiveApp.parseDirection(directions)}
  }

  "calculatePath" should "calculate the submarine path" in {

    val result = DiveApp.calculatePath(DiveApp.readDataFile("input.txt"))
    assertResult((2034,702)) { result}
    assertResult(1427868) {result._1 * result._2 }
  }

  /*
    forward 5 adds 5 to your horizontal position, a total of 5. Because your aim is 0, your depth does not change.
    down 5 adds 5 to your aim, resulting in a value of 5.
    forward 8 adds 8 to your horizontal position, a total of 13. Because your aim is 5, your depth increases by 8*5=40.
    up 3 decreases your aim by 3, resulting in a value of 2.
    down 8 adds 8 to your aim, resulting in a value of 10.
    forward 2 adds 2 to your horizontal position, a total of 15. Because your aim is 10, your depth increases by 2*10=20 to a total of 60.

    val directions = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

    hor = 5 + 8 = 13 + 2 = 15
    depth = 40 + 20 = 60
    aim = 5 - 3 = 2 + 8 = 10

    val directions1 = "down 5\nforward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

    down 5 aim = 5  // adds 5 to your aim
    forward 5: hor = 5 dept = 25 // adds value to your horizontal position,  depth = value * aim
    down 5: aim = 10 // adds 5 to your aim
    forward 8: hor 13 depth = 25 + (8 * 10) = 105 // adds value to your horizontal position,  depth = value * aim
    up 3: aim = aim - 3 = 7 // decreases your aim by value
    down 8: aim + 8 = 15 // adds 5 to your aim
    forward 2: hor = 15, depth = 105 + (2 * 15) = 135 // adds value to your horizontal position,  depth = depth + value * aim

   */
  "calculatePathWithAim" should "calculate the submarine path accounting for aim" in {

    val directions1 = "down 5\nforward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"
    val result = DiveApp.calculatePathWithAim(directions1)
    assertResult((15,135)) { result}
    assertResult(2025) {result._1 * result._2 }
  }

}
