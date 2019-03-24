import Board.Chessboard

object Party  {
  def main(args: Array[String]): Unit = {
    val c = Chessboard.initialisation()
    //Chessboard.printChessboard(c)

    c.squares.foreach(s => s.piece match {
      case Some(p) => p.movement(c)
      case None => ""
    })
  }
}
