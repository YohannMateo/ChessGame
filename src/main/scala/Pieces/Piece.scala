package Pieces

import Board.Color.Color
import Board.{Chessboard, Square}

trait Piece {
  val color:Color
  def printPiece() : String
  def movement(c : Chessboard) : List[Square]
}
