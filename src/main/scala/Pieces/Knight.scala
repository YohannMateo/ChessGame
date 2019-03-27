package Pieces

import Board.Color.ColorVal
import Board.{Chessboard, Square}

class Knight (override val color: ColorVal) extends Piece {
  val name = "Knight"
  override def printPiece(): String = name + " " + color

  override def movement(c: Chessboard):List[Square] = {
    val s = c.squares.find(s => s.piece match {
      case Some(p) => p==this
      case None => false
    })

    val list = s match {
      case Some(sq) =>
        c.squares.filter(s => conditionMovement(sq,s,color))
      case None => List[Square]()
    }

    //println("Knight " + color)
    //list.foreach(s => println(Square.printSquare(s)))
    list
  }

  private def conditionMovement(squareInit:Square,square:Square,color:ColorVal):Boolean = {
    square.piece match {
      case Some(p) =>
        if(color!=p.color) {
          (square.row == squareInit.row+1 && square.column == squareInit.column-2) || (square.row == squareInit.row+1 && square.column == squareInit.column+2) || // case diag mid high droite/gauche
            (square.row == squareInit.row+2 && square.column == squareInit.column+1) || (square.row == squareInit.row+2 && square.column == squareInit.column-1) || // case diag high droite/gauche
            (square.row == squareInit.row-1 && square.column == squareInit.column-2) || (square.row == squareInit.row-1 && square.column == squareInit.column+2) || // case diag mid down droite/gauche
            (square.row == squareInit.row-2 && square.column == squareInit.column+1) || (square.row == squareInit.row-2 && square.column == squareInit.column-1)  // case diag down droite/gauche
        } else {
          false
        }
      case None =>
        (square.row == squareInit.row+1 && square.column == squareInit.column-2) || (square.row == squareInit.row+1 && square.column == squareInit.column+2) || // case diag mid high droite/gauche
          (square.row == squareInit.row+2 && square.column == squareInit.column+1) || (square.row == squareInit.row+2 && square.column == squareInit.column-1) || // case diag high droite/gauche
          (square.row == squareInit.row-1 && square.column == squareInit.column-2) || (square.row == squareInit.row-1 && square.column == squareInit.column+2) || // case diag mid down droite/gauche
          (square.row == squareInit.row-2 && square.column == squareInit.column+1) || (square.row == squareInit.row-2 && square.column == squareInit.column-1)  // case diag down droite/gauche
    }

  }
}
