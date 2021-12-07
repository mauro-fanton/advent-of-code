import scala.+:
import scala.annotation.tailrec

class Bingo {
  def getLastWin(boards: Array[Board], draws: Array[Int], count: Int): Int = {

    val lastToWin = drawLastWinner(boards,draws,count).last
    val sumOfAllUnmarkedNumber = lastToWin._1.board.flatMap(o => o.filter(o => !lastToWin._2.contains(o))).sum

    sumOfAllUnmarkedNumber * lastToWin._2.last
  }


  def play(boards: Array[Board], draws: Array[Int], count: Int): Int = {

    val results = draw(boards,draws,count)

    val sumOfAllUnmarkedNumber = results._1.board.flatMap(o => o.filter(o => !results._2.contains(o))).sum
    sumOfAllUnmarkedNumber * results._2.last
  }

  def drawLastWinner(boards: Array[Board], draws: Array[Int], count: Int): Array[(Board, Array[Int])] = {

    val a = Array.ofDim[Array[MatchedDrawNumbers]](boards.length, 1)

    var previousWin: Array[(Board, Array[Int])] = Array()

    draws.foreach(n => {

      boards.zipWithIndex.foreach { case (b,i) =>

        val c = findNumberInBoard(b.board, n)

        if(a(i)(0) == null) a(i)(0) = c
        else a(i)(0) = a(i)(0) ++ c

        a.zipWithIndex.foreach { case (o,i) =>

          if(o(0) != null ) {
            val winningMatch = getScore(o(0), count)
            val containsBoard = previousWin.filter( o => o._1 == boards(i)).length > 0
            if (winningMatch.length >= count && !containsBoard)
              previousWin  = previousWin ++ Array((boards(i), o(0).map(o => o.value)))
          }
        }
      }

    })
    previousWin
  }

  def draw(boards: Array[Board], draws: Array[Int], count: Int): (Board,Array[Int]) = {

    val a = Array.ofDim[Array[MatchedDrawNumbers]](boards.length, 1)

    val score: (Board,Array[Int]) = (null , Array());
    draws.foreach(n => {

      if(score._2.length >= count) return score;
      boards.zipWithIndex.foreach { case (b,i) =>

        if(score._2.length >= count) return score;
        val c = findNumberInBoard(b.board, n)

        if(a(i)(0) == null) a(i)(0) = c
        else a(i)(0) = a(i)(0) ++ c

        a.zipWithIndex.foreach { case (o,i) =>

          if(o(0) != null ) {
            val winningMatch = getScore(o(0), count)
            if (winningMatch.length >= count)
              return (boards(i), o(0).map(o => o.value))
          }
        }
      }

    })
    score
  }

  def getScore(score: Array[MatchedDrawNumbers], maxBoardDim: Int): Array[Int] = {

    @tailrec
    def getWinningDraw(index: Int, score: Array[MatchedDrawNumbers], acc: Array[MatchedDrawNumbers], filter: (Array[MatchedDrawNumbers], Int) => Array[MatchedDrawNumbers]): Array[Int] = {

      if (acc.length == maxBoardDim || score.length == 0) acc.map(o => o.value)
      else getWinningDraw(index, score.tail, filter(score, index), filter)
    }

    score.flatMap(o => {
      var result = getWinningDraw(o.row, score, Array(), (s, i) => s.filter(o => o.row == i))
      if (result.length == maxBoardDim) return result

      result = getWinningDraw(o.col, score, Array(), (s, i) => s.filter(o => o.col == i))

      if (result.length == maxBoardDim) return result else Array()
    })
  }

  def findNumberInBoard(board: Array[Array[Int]], number: Int): Array[MatchedDrawNumbers] = {

    board.zipWithIndex.map {
      case (row, i) => row.zipWithIndex.map {
        case (`number`, j) => MatchedDrawNumbers(i, j, number)
        case(_, j) => MatchedDrawNumbers(-1,-1,-1)} }
      .flatMap( o => o)
      .filter {
        case MatchedDrawNumbers(-1,-1,-1) => false
        case _ => true
      }
  }
}

object Bingo {
  def apply(): Bingo = {
    new Bingo()
  }
}
