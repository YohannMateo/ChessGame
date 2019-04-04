package Pieces

import Board.Color.ColorVal
import Board.{Chessboard, Square}

case class King (override val color: ColorVal, override val name: String = "King") extends Piece

object King {
  def printPiece(p : King): String = p.name + " " + p.color

  def movement(k : King, c: Chessboard):List[Square] = {
    val s = c.squares.find(s => s.piece match {
      case Some(p) => p==k
      case None => false
    })

    val list = s match {
      case Some(sq) =>
        c.squares.filter(s => conditionMovement(sq,s))
      case None => List[Square]()
    }

    //println("King " + color)
    //list.foreach(s => println(Square.printSquare(s)))
    list
  }

  private def conditionMovement(squareInit:Square,square:Square):Boolean = {
    val color = squareInit.piece match {
      case Some(p) => p.color
    }

    square.piece match {
      case Some(p) =>
        if(color!=p.color) {
          (square.row == squareInit.row+1 && square.column == squareInit.column) || (square.row == squareInit.row-1 && square.column == squareInit.column) || // case devant/derriere
            (square.row == squareInit.row+1 && square.column == squareInit.column+1) || (square.row == squareInit.row+1 && square.column == squareInit.column-1) || // case diagonale haut droite/gauche
            (square.row == squareInit.row-1 && square.column == squareInit.column+1) || (square.row == squareInit.row-1 && square.column == squareInit.column-1) || // case diagonale bas droite/gauche
            (square.row == squareInit.row && square.column == squareInit.column+1) || (square.row == squareInit.row && square.column == squareInit.column-1) // case droite/gauche
        } else {
          false
        }
      case None =>
        (square.row == squareInit.row+1 && square.column == squareInit.column) || (square.row == squareInit.row-1 && square.column == squareInit.column) || // case devant/derriere
          (square.row == squareInit.row+1 && square.column == squareInit.column+1) || (square.row == squareInit.row+1 && square.column == squareInit.column-1) || // case diagonale haut droite/gauche
          (square.row == squareInit.row-1 && square.column == squareInit.column+1) || (square.row == squareInit.row-1 && square.column == squareInit.column-1) || // case diagonale bas droite/gauche
          (square.row == squareInit.row && square.column == squareInit.column+1) || (square.row == squareInit.row && square.column == squareInit.column-1) // case droite/gauche
    }

  }
}