package Pieces

import Board.Color.Color
import Board.{Chessboard, Square}

class King (color: Color) extends Piece {
  val name = "King"
  override def printPiece(): String = name + " " + color

  override def movement(c: Chessboard):List[Square] = {
    val s = c.squares.find(s => s.piece match {
      case Some(p) => p==this
      case None => false
    })

    val list = s match {
      case Some(sq) =>
        c.squares.filter(s => conditionMovement(sq,s))
      case None => List[Square]()
    }

    list
  }

  private def conditionMovement(squareInit:Square,square:Square):Boolean = {
    (square.row == squareInit.row+1 && square.column == squareInit.column) || (square.row == squareInit.row-1 && square.column == squareInit.column) || // case devant/derriere
      (square.row == squareInit.row+1 && square.column == squareInit.column+1) || (square.row == squareInit.row+1 && square.column == squareInit.column-1) || // case diagonale haut droite/gauche
      (square.row == squareInit.row-1 && square.column == squareInit.column+1) || (square.row == squareInit.row-1 && square.column == squareInit.column-1) || // case diagonale bas droite/gauche
      (square.row == squareInit.row && square.column == squareInit.column+1) || (square.row == squareInit.row && square.column == squareInit.column-1) // case droite/gauche
  }
}