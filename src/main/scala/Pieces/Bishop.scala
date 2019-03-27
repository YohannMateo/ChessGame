package Pieces

import Board.Color.ColorVal
import Board.{Chessboard, Square}

class Bishop (override val color: ColorVal) extends Piece {
  val name = "Bishop"
  override def printPiece(): String = name + " " + color

  override def movement(c: Chessboard):List[Square] = {
    val s = c.squares.find(s => s.piece match {
      case Some(p) => p==this
      case None => false
    })

    val list = s match {
      case Some(sq) =>
        val allCase = c.squares.filter(s => (s.row,s.column) != (sq.row,sq.column) && conditionMovement(sq,s))
        accessibleSquares(sq,allCase)
      case None => List[Square]()
    }

    //println("Bishop " + color)
    //list.foreach(s => println(Square.printSquare(s)))
    list
  }

  private def conditionMovement(squareInit:Square,square:Square):Boolean = {
    square.row - squareInit.row == square.column - squareInit.column ||
      square.row - squareInit.row == -(square.column - squareInit.column) ||
      -(square.row - squareInit.row) == square.column - squareInit.column // diagonales
  }

  private def accessibleSquares(squareInit:Square, allCase:List[Square]): List[Square] = {
    val firstPieceHautGauche = allCase.filter(s=> (s.column < squareInit.column) && s.row > squareInit.row).sortBy(s=>s.column).reverse.find(s => s.piece match {
      case Some(_) => true
      case None => false
    })
    val firstPieceHautDroite = allCase.filter(s=> (s.column > squareInit.column) && s.row > squareInit.row).sortBy(s=>s.column).reverse.find(s => s.piece match {
      case Some(_) => true
      case None => false
    })
    val firstPieceBasGauche = allCase.filter(s=> (s.row < squareInit.row) && s.column < squareInit.column).sortBy(s=>s.row).reverse.find(s => s.piece match {
      case Some(_) => true
      case None => false
    })
    val firstPieceBasDroite = allCase.filter(s=> (s.row < squareInit.row) && s.column > squareInit.column).sortBy(s=>s.row).reverse.find(s => s.piece match {
      case Some(_) => true
      case None => false
    })

    val listHautGauche = firstPieceHautGauche match {
      case Some(s) => s.piece match {
        case Some(p) =>
          if(color!=p.color) {
            allCase.filter(sq=>(s.column <= sq.column) && s.row >= sq.row)
          } else {
            allCase.filter(sq=>(s.column < sq.column) && s.row > sq.row)
          }
        case None => List[Square]()
      }
      case None => allCase.filter(sq=>(squareInit.column > sq.column) && squareInit.row < sq.row)
    }

    val listHautDroite = firstPieceHautDroite match {
      case Some(s) => s.piece match {
        case Some(p) =>
          if(color!=p.color) {
            allCase.filter(sq=>(s.column >= sq.column) && s.row >= sq.row)
          } else {
            allCase.filter(sq=>(s.column > sq.column) && s.row > sq.row)
          }
        case None => List[Square]()
      }
      case None => allCase.filter(sq=>(squareInit.column < sq.column) && squareInit.row < sq.row)
    }

    val listBasGauche = firstPieceBasGauche match {
      case Some(s) => s.piece match {
        case Some(p) =>
          if(color!=p.color) {
            allCase.filter(sq=>(s.row <= sq.row) && s.column <= sq.column)
          } else {
            allCase.filter(sq=>(s.row < sq.row) && s.column < sq.column)
          }
        case None => List[Square]()
      }
      case None => allCase.filter(sq=>(squareInit.column > sq.column) && squareInit.row > sq.row)
    }

    val listBasDroite = firstPieceBasDroite match {
      case Some(s) => s.piece match {
        case Some(p) =>
          if(color!=p.color) {
            allCase.filter(sq=>(s.row <= sq.row) && s.column >= sq.column)
          } else {
            allCase.filter(sq=>(s.row < sq.row) && s.column > sq.column)
          }
        case None => List[Square]()
      }
      case None => allCase.filter(sq=>(squareInit.column < sq.column) && squareInit.row > sq.row)
    }

    List.concat(listBasDroite,listBasGauche,listHautDroite,listHautGauche)
  }
}
