package Board

import Board.Color.{Black, White}
import Pieces._

case class Chessboard(squares : List[Square])

object Chessboard {
  def initialisation():Chessboard = {
    val listPawn = List.range(1,9).flatMap(i => List(Square(2,i,Option(new Pawn(White))), Square(7,i,Option(new Pawn(Black)))))
    val listRook = List(Square(1,1,Option(new Rook(White))), Square(1,8,Option(new Rook(White))),Square(8,1,Option(new Rook(Black))), Square(8,8,Option(new Rook(Black))))
    val listKnight = List(Square(1,2,Option(new Knight(White))), Square(1,7,Option(new Knight(White))),Square(8,2,Option(new Knight(Black))), Square(8,7,Option(new Knight(Black))))
    val listBishop = List(Square(1,3,Option(new Bishop(White))), Square(1,6,Option(new Bishop(White))),Square(8,3,Option(new Bishop(Black))), Square(8,6,Option(new Bishop(Black))))
    val listQueen = List(Square(1,4,Option(new Queen(White))), Square(8,4,Option(new Queen(Black))))
    val listKing = List(Square(1,5,Option(new King(White))), Square(8,5,Option(new King(Black))))
    val listEmpty = List.range(1,9).flatMap(i => List(Square(6,i,None), Square(5,i,None), Square(4,i,None), Square(3,i,None)))

    val listPieces = List.concat(listPawn,listBishop,listEmpty,listKing,listKnight,listQueen,listRook).sortBy(s=>(s.row,s.column))
    Chessboard(listPieces)
  }

  def printChessboard (c : Chessboard) = c.squares.foreach(s => {
    println(Square.printSquare(s))
    s.piece match {
      case Some(p) => PieceMovement.movement(p,c)
      case None => println("Pas de piece")
    }
  })
}