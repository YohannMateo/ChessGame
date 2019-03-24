package Pieces

import Board.Color.Color
import Board.{Chessboard, Square}

class Rook (color: Color) extends Piece {
  val name = "Rook"
  override def printPiece(): String = name + " " + color

  override def movement(c: Chessboard):List[Square] = {
    val s = c.squares.find(s => s.piece match {
      case Some(p) => p==this
      case None => false
    })
    println(s match {
      case Some(sq) => "" + this + Square.printSquare(sq)
      case None => "Pas de piece"
    })
    List[Square]()
  }
}
