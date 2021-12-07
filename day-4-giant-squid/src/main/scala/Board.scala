case class Board(board: Array[Array[Int]])

object Board {
  def apply(board: Array[Array[Int]]) = {
    new Board(board)
  }
}
