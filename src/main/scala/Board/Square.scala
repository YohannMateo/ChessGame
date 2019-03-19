package Board

import Pieces.Piece

class Square (val row: Int, val column: Int) {
  var col = ""

  if (row%2 == 0 && column%2==1 || row%2 == 1 && column%2==0)
    col = "white"
  else
    col = "black"

  val color = col


  var piece: Option[Piece] = None;

  def print() {
    var p = piece match {
      case Some(n) => n.print()
      case None => "  Piece : empty"
    }
    println("row : " + row + " --  column : " + column + " -- " + p)
  }

  def setPiece(p: Piece) {
    piece = Option(p)
  }

  def getPiece() = {
    piece
  }
}
