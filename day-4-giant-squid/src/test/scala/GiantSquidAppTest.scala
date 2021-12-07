import org.scalatest.flatspec.AnyFlatSpec

class GiantSquidAppTest extends AnyFlatSpec {

  val board1 = Array(
    Array(22, 13, 17, 11,  0),
    Array(8, 2, 23,  4, 24),
    Array(21, 9, 14, 16,  7),
    Array(6, 10,  3, 18,  5),
    Array(1, 12, 20, 15, 19)
  )

  val board2= Array(
    Array(3, 15, 0, 2, 22),
    Array(9, 18, 13, 17, 5),
    Array(19, 8, 7, 25, 23),
    Array(20, 11, 10, 24, 4),
    Array(14, 21, 16, 12,  6)
  )

  val board3= Array(
    Array(14, 21, 17, 24, 4),
    Array(10, 16, 15, 9, 19),
    Array(18, 8, 23, 26, 20),
    Array(22, 11, 13, 6, 5),
    Array(2, 0, 12, 3, 7)
  )

  val draw = Array(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)



  "play" should "return the final score" in {
    val boards = Array(
      Board(board1),
      Board(board2),
      Board(board3)
    )

    assertResult(4512) {Bingo().play(boards, draw, 5)}
  }

  "getLastWin" should "return last board to win" in {
    val boards = Array(
      Board(board1),
      Board(board2),
      Board(board3)
    )

    assertResult(1924) {Bingo().getLastWin(boards, draw, 5)}
  }

  "getScore" should "return the winning row" in {
    val matchedNumbersOnBoard = Array(
      MatchedDrawNumbers(0,0,5),
      MatchedDrawNumbers(0, 1, 8),
      MatchedDrawNumbers(1,0, 7),
      MatchedDrawNumbers(0,2, 4)
    )
    assertResult(Array(5,8,4)) {Bingo().getScore(matchedNumbersOnBoard, 3) }
  }

  "getScore" should "return no winning row" in {
    val matchedNumbersOnBoard = Array(
      MatchedDrawNumbers(1,0,5),
      MatchedDrawNumbers(0, 1, 8),
      MatchedDrawNumbers(1,0, 7),
      MatchedDrawNumbers(0,2, 4)
    )
    assertResult(Array()) { Bingo().getScore(matchedNumbersOnBoard,3) }
  }

  /*
    0,0 | 0,1 | 0,2
    1,0 | 1,1 | 1,2
    2,0 | 2,1 | 2,2
   */
  "getScore" should "return the winning column" in {
    val matchedNumbersOnBoard = Array(
      MatchedDrawNumbers(1,1,5),
      MatchedDrawNumbers(0, 1, 8),
      MatchedDrawNumbers(2,1, 7),
      MatchedDrawNumbers(0,2, 4)
    )
    assertResult(Array(5,8,7)) { Bingo().getScore(matchedNumbersOnBoard,3) }
  }

  /*
   row=0,col=0            | row=0,col=1 value=(8)   | row=0,col=2 value=(4)
   row=1,col=0 value=(69) | row=1,col=1 value=(5)   | row=1,col=2
   row=2,col=0 value=(24) | row=2,col=1 value=(7)   | row=2,col=2
   row=3,col=0            | row=3,col=1 value=(12)  | row=3,col=2

  */
  "getScore" should "return the winning column for an asymmetric board" in {
    val matchedNumbersOnBoard = Array(
      MatchedDrawNumbers(1,1,5),
      MatchedDrawNumbers(0, 1, 8),
      MatchedDrawNumbers(2,1, 7),
      MatchedDrawNumbers(0,2, 4),
      MatchedDrawNumbers(3,1,12),
      MatchedDrawNumbers(2,0, 24),
      MatchedDrawNumbers(1,0, 69)
    )
    assertResult(Array(5,8,7, 12)) { Bingo().getScore(matchedNumbersOnBoard,4) }
  }


}
