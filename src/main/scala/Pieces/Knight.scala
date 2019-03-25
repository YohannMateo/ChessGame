package Pieces

import Board.Color.Color
import Board.{Chessboard, Square}

class Knight (color: Color) extends Piece {
  val name = "Knight"
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

    //println("Knight " + color)
    //list.foreach(s => println(Square.printSquare(s)))
    list
  }

  private def conditionMovement(squareInit:Square,square:Square):Boolean = {
    (square.row == squareInit.row+1 && square.column == squareInit.column-2) || (square.row == squareInit.row+1 && square.column == squareInit.column+2) || // case diag mid high droite/gauche
      (square.row == squareInit.row+2 && square.column == squareInit.column+1) || (square.row == squareInit.row+2 && square.column == squareInit.column-1) || // case diag high droite/gauche
      (square.row == squareInit.row-1 && square.column == squareInit.column-2) || (square.row == squareInit.row-1 && square.column == squareInit.column+2) || // case diag mid down droite/gauche
      (square.row == squareInit.row-2 && square.column == squareInit.column+1) || (square.row == squareInit.row-2 && square.column == squareInit.column-1)  // case diag down droite/gauche
  }
}
