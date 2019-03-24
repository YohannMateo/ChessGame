package Pieces

import Board.Color.Color
import Board.{Chessboard, Color, Square}

class Pawn (color: Color) extends Piece {
  val name = "Pawn"
  override def printPiece(): String = name + " " + color

  override def movement(c: Chessboard):List[Square] = {
    val s = c.squares.find(s => s.piece match {
      case Some(p) => p==this
      case None => false
    })

    val list = s match {
      case Some(sq) =>
        color match {
          case Color.White =>
            if (sq.row==2) {
              c.squares.filter(s => (s.row == 3 || sq.row == 4) && s.column == sq.column)
            } else {
              c.squares.filter(s => s.row == sq.row+1 && s.column == sq.column)
            }
          case Color.Black =>
            if (sq.row==7) {
              c.squares.filter(s => (s.row == 6 || sq.row == 5) && s.column == sq.column)
            } else {
              c.squares.filter(s => s.row == sq.row-1 && s.column == sq.column)
            }
        }
      case None => List[Square]()
    }

    list

  }
}