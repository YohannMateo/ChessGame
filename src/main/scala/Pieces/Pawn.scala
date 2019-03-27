package Pieces

import Board.Color.{Black, ColorVal, White}
import Board.{Chessboard, Square}

class Pawn (override val color: ColorVal) extends Piece {
  val name = "Pawn"
  override def printPiece(): String = name + " " + color

  override def movement(c: Chessboard):List[Square] = {
    val s = c.squares.find(s => s.piece match {
      case Some(p) => p==this
      case None => false
    })

    val list = s match {
      case Some(sq) => c.squares.filter(s => conditionMovement(sq,s,color))
      case None => List[Square]()
    }

    //println("Pawn " + color)
    //list.foreach(s => println(Square.printSquare(s)))
    println("Pawn : ")
    println(list)
    list
  }

  private def conditionMovement(squareInit:Square,square:Square,color: ColorVal):Boolean = {
    square.piece match {
      case Some(p) =>
        (color,p.color) match {
          case (White,Black) =>
            square.row == squareInit.row+1 && (square.column == squareInit.column+1 || square.column == squareInit.column-1)
          case (Black,White) =>
            square.row == squareInit.row-1 && (square.column == squareInit.column+1 || square.column == squareInit.column-1)
          case (_,_) => false
        }
      case None =>
        color match {
          case White =>
            if (squareInit.row == 2) {
              (square.row == squareInit.row+1 || square.row==squareInit.row+2) && square.column == squareInit.column
            } else {
              square.row == squareInit.row+1 && square.column == squareInit.column
            }
          case Black =>
            if (squareInit.row == 7) {
              (square.row == squareInit.row-1 || square.row==squareInit.row-2) && square.column == squareInit.column
            } else {
              square.row == squareInit.row-1 && square.column == squareInit.column
            }
        }
    }
  }
}