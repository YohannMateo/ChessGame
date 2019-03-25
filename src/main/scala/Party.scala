import Board.Chessboard

object Party  {
  def main(args: Array[String]) = {
    val c = Chessboard.initialisation()
    Chessboard.printChessboard(c)

  }
}
