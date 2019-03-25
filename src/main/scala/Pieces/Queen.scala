package Pieces

import Board.Color.Color
import Board.{Chessboard, Square}

class Queen (color: Color) extends Piece {
  val name = "Queen"
  override def printPiece(): String = name + " " + color

  override def movement(c: Chessboard):List[Square] = {
    val s = c.squares.find(s => s.piece match {
      case Some(p) => p==this
      case None => false
    })

    val list = s match {
      case Some(sq) =>
        c.squares.filter(s => (s.row,s.column) != (sq.row,sq.column) && conditionMovement(sq,s))
      case None => List[Square]()
    }

    //println("Queen " + color)
    //list.foreach(s => println(Square.printSquare(s)))
    list
  }

  private def conditionMovement(squareInit:Square,square:Square):Boolean = {
    square.row == squareInit.row || square.column == squareInit.column || // case diag mid high droite/gauche
      square.row - squareInit.row == square.column - squareInit.column ||
      square.row - squareInit.row == -(square.column - squareInit.column) ||
      -(square.row - squareInit.row) == square.column - squareInit.column
  }
}