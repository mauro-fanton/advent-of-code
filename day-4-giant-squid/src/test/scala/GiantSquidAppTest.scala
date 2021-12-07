import org.scalatest.flatspec.AnyFlatSpec

import scala.annotation.tailrec

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

  case class MatchedDrawNumbers(row: Int, col: Int, value: Int)

  def play(board: Array[Array[Int]], draws: Array[Int]): Array[Int] = {

          @tailrec
          def play1(board: Array[Array[Int]], draws: Array[Int], acc: Array[(Int,Int,Int)]): Array[Int] = {

            val maxDim = Math.max(board(0).length, board.length)
            val score = getScore(acc, maxDim)

            if(score.length == maxDim) return score
            if( draws.isEmpty ) return score

            val n = draws.head

            val found = board.zipWithIndex.map {
              case (row, i) => row.zipWithIndex.map {
                case (`n`, j) => (i, j, n)
                case(_, j) => (-1,-1,-1)} }
              .flatMap( o => o)
              .filter(o => o != (-1,-1,-1))
//              val found = board.zipWithIndex.map {
//                case (row, i) => row.zipWithIndex.map {
//                  case (`n`, j) => MatchedDrawNumbers(i, j, n)
//                  case(_, j) => MatchedDrawNumbers(-1,-1,-1)} }
//                .flatMap( o => o)
//                .filter {case MatchedDrawNumbers(-1,-1,-1) => false
//                case _ => true}

            play1(board, draws.drop(1), acc ++ found)
          }

         play1(board, draws, Array())

  }

  def getScore(score: Array[(Int, Int,Int)], maxBoardDim: Int): Array[Int] = {

    @tailrec
    def getWinningDraw(index: Int, score: Array[(Int, Int,Int)], acc: Array[(Int,Int,Int)], filter: (Array[(Int,Int,Int)],Int) => Array[(Int,Int,Int)]): Array[Int]  = {

      if(acc.length == maxBoardDim || score.length  == 0)  acc.map(o => o._3)
      else getWinningDraw(index, score.tail, filter(score, index), filter)
    }

    score.flatMap(o => {
      var result = getWinningDraw(o._1, score, Array(), (s,i) => s.filter(o => o._1 == i))
      if(result.length == maxBoardDim) return result

      result = getWinningDraw(o._2, score, Array(), (s,i) => s.filter(o => o._2 == i))

      if(result.length == maxBoardDim) return result else Array()
    })
  }

  val draw = Array(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)


  "play" should "return the winning number for one board" in {
    assert(Array(14, 21, 17, 24, 4).sorted sameElements play(board3, draw).sorted)
  }


  "play" should "return the winning draw for one board" in {
    val board = Array(Array(4,9,5), Array(6, 8, 10))
    assertResult(Array(6, 8, 10)) { play(board, Array(6,8,10)) }

  }

  "play" should "return no winning draw for one board" in {
    val board = Array(Array(4,9,5), Array(6, 8, 10))
    assertResult(Array()) { play(board, Array(6,8)) }

  }

  "play" should "return the found numbers in a column for one board" in {
    val board = Array(Array(4,9,5), Array(6, 8, 10))
    assertResult(Array()) { play(board, Array(4,6)) }

  }

  "getScore" should "return the winning row" in {
    val matchedNumbersOnBoard = Array((0,0,5), (0, 1, 8), (1,0, 7), (0,2, 4))
    assertResult(Array(5,8,4)) {getScore(matchedNumbersOnBoard, 3) }
  }

  "getScore" should "return no winning row" in {
    val matchedNumbersOnBoard = Array((1,0,5), (0, 1, 8), (1,0, 7), (0,2, 4))
    assertResult(Array()) { getScore(matchedNumbersOnBoard,3) }
  }

  /*
    0,0 | 0,1 | 0,2
    1,0 | 1,1 | 1,2
    2,0 | 2,1 | 2,2
   */
  "getScore" should "return the winning column" in {
    val matchedNumbersOnBoard = Array((1,1,5), (0, 1, 8), (2,1, 7), (0,2, 4))
    assertResult(Array(5,8,7)) { getScore(matchedNumbersOnBoard,3) }
  }

  /*
   row=0,col=0            | row=0,col=1 value=(8)   | row=0,col=2 value=(4)
   row=1,col=0 value=(69) | row=1,col=1 value=(5)   | row=1,col=2
   row=2,col=0 value=(24) | row=2,col=1 value=(7)   | row=2,col=2
   row=3,col=0            | row=3,col=1 value=(12)  | row=3,col=2

  */
  "getScore" should "return the winning column for an asymmetric board" in {
    val matchedNumbersOnBoard = Array((1,1,5), (0, 1, 8), (2,1, 7), (0,2, 4), (3,1,12), (2,0, 24), (1,0, 69))
    assertResult(Array(5,8,7, 12)) { getScore(matchedNumbersOnBoard,4) }
  }
}
