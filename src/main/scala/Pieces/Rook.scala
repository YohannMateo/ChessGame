package Pieces

import Board.{Chessboard, Square}

class Rook (override val color: _root_.Board.Color.Color) extends Piece {
  val name = "Rook"
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
    //println("Rook " + color)
    //list.foreach(s => println(Square.printSquare(s)))
    list
  }

  private def conditionMovement(squareInit:Square,square:Square):Boolean = {
    square.row == squareInit.row || square.column == squareInit.column // case droite/gauche haut/bas
  }

  private def accessibleSquares(squareInit:Square, allCase:List[Square]): List[Square] = {
    val firstPieceGauche = allCase.filter(s=> (s.column < squareInit.column) && s.row == squareInit.row).sortBy(s=>s.column).reverse.find(s => s.piece match {
      case Some(_) => true
      case None => false
    })
    val firstPieceDroite = allCase.filter(s=> (s.column > squareInit.column) && s.row == squareInit.row).sortBy(s=>s.column).find(s => s.piece match {
      case Some(_) => true
      case None => false
    })
    val firstPieceHaut = allCase.filter(s=> (s.row > squareInit.row) && s.column == squareInit.column).sortBy(s=>s.row).find(s => s.piece match {
      case Some(_) => true
      case None => false
    })
    val firstPieceBas = allCase.filter(s=> (s.row < squareInit.row) && s.column == squareInit.column).sortBy(s=>s.row).reverse.find(s => s.piece match {
      case Some(_) => true
      case None => false
    })

    val listGauche = firstPieceGauche match {
      case Some(s) => s.piece match {
        case Some(p) =>
          if(color!=p.color) {
            allCase.filter(sq=>(s.column <= sq.column && sq.column<=squareInit.column) && s.row == sq.row)
          } else {
            allCase.filter(sq=>(s.column < sq.column && sq.column<squareInit.column) && s.row == sq.row)
          }
        case None => List[Square]()
      }
      case None => allCase.filter(sq=>(sq.column<squareInit.column) && squareInit.row == sq.row)
    }

    val listDroite = firstPieceDroite match {
      case Some(s) => s.piece match {
        case Some(p) =>
          if(color!=p.color) {
            allCase.filter(sq=>(s.column >= sq.column && sq.column>=squareInit.column) && s.row == sq.row)
          } else {
            allCase.filter(sq=>(s.column > sq.column && sq.column>squareInit.column) && s.row == sq.row)
          }
        case None => List[Square]()
      }
      case None => allCase.filter(sq=>(sq.column>squareInit.column) && squareInit.row == sq.row)
    }

    val listHaut = firstPieceHaut match {
      case Some(s) => s.piece match {
        case Some(p) =>
          if(color!=p.color) {
            allCase.filter(sq=>(s.row >= sq.row && sq.row>=squareInit.row) && s.column == sq.column)
          } else {
            allCase.filter(sq=>(s.row > sq.row && sq.row>squareInit.row) && s.column == sq.column)
          }
        case None => List[Square]()
      }
      case None => allCase.filter(sq=>(sq.row>squareInit.row) && squareInit.column == sq.column)
    }

    val listBas = firstPieceBas match {
      case Some(s) => s.piece match {
        case Some(p) =>
          if(color!=p.color) {
            allCase.filter(sq=>(s.row <= sq.row && sq.row <= squareInit.row) && s.column == sq.column)
          } else {
            allCase.filter(sq=>(s.row < sq.row && sq.row < squareInit.row) && s.column == sq.column)
          }
        case None => List[Square]()
      }
      case None => allCase.filter(sq=>(sq.row<squareInit.row) && squareInit.column == sq.column)
    }

    List.concat(listBas,listDroite,listGauche,listHaut)
  }
}
