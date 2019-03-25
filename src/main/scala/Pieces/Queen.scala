package Pieces

import Board.{Chessboard, Square}

class Queen (override val color: _root_.Board.Color.Color) extends Piece {
  val name = "Queen"
  override def printPiece(): String = name + " " + color

  override def movement(c: Chessboard):List[Square] = {
    val s = c.squares.find(s => s.piece match {
      case Some(p) => p==this
      case None => false
    })

    val list = s match {
      case Some(sq) =>
        val allCaseRook = c.squares.filter(s => (s.row,s.column) != (sq.row,sq.column) && conditionMovementRook(sq,s))
        val allCaseBishop = c.squares.filter(s => (s.row,s.column) != (sq.row,sq.column) && conditionMovementBishop(sq,s))
        List.concat(accessibleSquaresBishop(sq,allCaseBishop),accessibleSquaresRook(sq,allCaseRook))
      case None => List[Square]()
    }

    //println("Queen " + color)
    //list.foreach(s => println(Square.printSquare(s)))
    list
  }

  private def conditionMovementRook(squareInit:Square,square:Square):Boolean = {
    square.row == squareInit.row || square.column == squareInit.column // case droite/gauche haut/bas
  }

  private def conditionMovementBishop(squareInit:Square,square:Square):Boolean = {
    square.row - squareInit.row == square.column - squareInit.column ||
      square.row - squareInit.row == -(square.column - squareInit.column) ||
      -(square.row - squareInit.row) == square.column - squareInit.column // diagonales
  }

  private def accessibleSquaresRook(squareInit:Square, allCase:List[Square]): List[Square] = {
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

  private def accessibleSquaresBishop(squareInit:Square, allCase:List[Square]): List[Square] = {
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