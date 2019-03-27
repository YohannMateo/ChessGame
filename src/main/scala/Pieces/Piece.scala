package Pieces

import Board.Color.ColorVal
import Board.{Chessboard, Square}

trait Piece {
  val color:ColorVal
  def printPiece() : String
  def movement(c : Chessboard) : List[Square]
}
