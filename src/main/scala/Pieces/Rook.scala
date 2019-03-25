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

    val list = s match {
      case Some(sq) =>
        c.squares.filter(s => conditionMovement(sq,s))
      case None => List[Square]()
    }

    //println("Rook " + color)
    //list.foreach(s => println(Square.printSquare(s)))
    list
  }

  private def conditionMovement(squareInit:Square,square:Square):Boolean = {
    square.row == squareInit.row || square.column == squareInit.column // case droite/gauche haut/bas

  }
}
