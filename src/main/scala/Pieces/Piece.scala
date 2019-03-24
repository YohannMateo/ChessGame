package Pieces

import Board.{Chessboard, Square}

trait Piece {
  def printPiece() : String
  def movement(c : Chessboard) : List[Square]
}
