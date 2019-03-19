package Board

import Pieces._

import scala.collection.mutable.ListBuffer

class Chessboard {
  var squares = new ListBuffer[Square]()

  for (i <- 1 until 9)
    for (j <- 1 until 9)
      squares += new Square(i,j)


  private val allSquare = squares.toList;

  var pieces = new ListBuffer[Piece]()
  val black = new Player("black")
  val white = new Player("white")
  for (i <- 1 until 9) {
    var squareBlack = squares.find(s => (s.column == i && s.row == 7))
    var squareWhite = squares.find(s => (s.column == i && s.row == 2))
    (squareWhite,squareBlack) match {
      case (Some(w),Some(b)) =>
        var pawn = new Pawn(white,w,"white")
        w.setPiece(pawn)
        pieces += pawn;
        pawn = new Pawn(black,b,"black")
        b.setPiece(pawn)
        pieces += pawn;
      case _ => "Squares not found"
    }

    squareBlack = squares.find(s => (s.column == i && s.row == 8))
    squareWhite = squares.find(s => (s.column == i && s.row == 1))
    i match {
      case 1 | 8 =>
        (squareWhite,squareBlack) match {
          case (Some(w), Some(b)) =>
            var pawn = new Rook(white, w, "white")
            w.setPiece(pawn)
            pieces += pawn;
            pawn = new Rook(black, b, "black")
            b.setPiece(pawn)
            pieces += pawn;
          case _ => "Squares not found"
        }
      case 2 | 7 =>
        (squareWhite,squareBlack) match {
          case (Some(w), Some(b)) =>
            var pawn = new Knight(white, w, "white")
            w.setPiece(pawn)
            pieces += pawn;
            pawn = new Knight(black, b, "black")
            b.setPiece(pawn)
            pieces += pawn;
          case _ => "Squares not found"
        }
      case 3 | 6 =>
        (squareWhite,squareBlack) match {
          case (Some(w), Some(b)) =>
            var pawn = new Bishop(white, w, "white")
            w.setPiece(pawn)
            pieces += pawn;
            pawn = new Bishop(black, b, "black")
            b.setPiece(pawn)
            pieces += pawn;
          case _ => "Squares not found"
        }
      case 4 =>
        (squareWhite,squareBlack) match {
          case (Some(w), Some(b)) =>
            var pawn = new Queen(white, w, "white")
            w.setPiece(pawn)
            pieces += pawn;
            pawn = new Queen(black, b, "black")
            b.setPiece(pawn)
            pieces += pawn;
          case _ => "Squares not found"
        }
      case 5 =>
        (squareWhite,squareBlack) match {
          case (Some(w), Some(b)) =>
            var pawn = new King(white, w, "white")
            w.setPiece(pawn)
            pieces += pawn;
            pawn = new King(black, b, "black")
            b.setPiece(pawn)
            pieces += pawn;
          case _ => "Squares not found"
        }
      case _ => "Index Invalid"

    }
  }

  def print() {
    allSquare.foreach {x => x.print()}
  }
}
