package Board

import Pieces._

case class Chessboard(squares : List[Square])

object Chessboard {
  def initialisation():Chessboard = {
    val listPawn = List.range(1,9).flatMap(i => List(Square(2,i,Option(new Pawn(Color.White))), Square(7,i,Option(new Pawn(Color.Black)))))
    val listRook = List(Square(1,1,Option(new Rook(Color.White))), Square(1,8,Option(new Rook(Color.White))),Square(8,1,Option(new Rook(Color.Black))), Square(8,8,Option(new Rook(Color.Black))))
    val listKnight = List(Square(1,2,Option(new Knight(Color.White))), Square(1,7,Option(new Knight(Color.White))),Square(8,2,Option(new Knight(Color.Black))), Square(8,7,Option(new Knight(Color.Black))))
    val listBishop = List(Square(1,3,Option(new Bishop(Color.White))), Square(1,6,Option(new Bishop(Color.White))),Square(8,3,Option(new Bishop(Color.Black))), Square(8,6,Option(new Bishop(Color.Black))))
    val listQueen = List(Square(1,4,Option(new Queen(Color.White))), Square(8,4,Option(new Queen(Color.Black))))
    val listKing = List(Square(1,5,Option(new King(Color.White))), Square(8,4,Option(new King(Color.Black))))
    val listEmpty = List.range(1,9).flatMap(i => List(Square(6,i,None), Square(5,i,None), Square(4,i,None), Square(3,i,None)))

    val listPieces = List.concat(listPawn,listBishop,listEmpty,listKing,listKnight,listQueen,listRook).sortBy(s=>(s.row,s.column))
    Chessboard(listPieces)
  }

  def printChessboard (c : Chessboard) = c.squares.foreach(s => {
    println(Square.printSquare(s))
    s.piece match {
      case Some(p) => println(p.movement(c))
      case None => println("Pas de piece")
    }
  })
}