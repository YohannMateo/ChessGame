package Board

import Pieces._

case class Square (row: Int, column: Int, piece: Option[Piece])

object Square {
  def addPiece(s: Square, p : Piece): Square = s.copy(piece=Some(p))

  def printSquare(s: Square):String = {

    s.piece match {
      case Some(p) => "Square : (row : " + s.row + ", column : " + s.column + ") include " + PieceMovement.printPiece(p)
      case None => "Square : (row : " + s.row + ", column : " + s.column + ") without piece"
    }
  }
}
