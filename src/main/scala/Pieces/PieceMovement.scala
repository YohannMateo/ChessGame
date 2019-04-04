package Pieces

import Board.{Chessboard, Square}

trait PieceMovement[A] {
  def movement(a : A, c : Chessboard):List[Square]
  def printPiece(a : A):String
}

object PieceMovement {
  def movement[A](a: A, c : Chessboard)(implicit move: PieceMovement[A]) = move.movement(a,c)

  def printPiece[A](a: A)(implicit print: PieceMovement[A]) = print.printPiece(a)

  private implicit val kingMove = new PieceMovement[King] {
    def movement(k: King, c: Chessboard): List[Square] = King.movement(k,c)

    def printPiece(k: King): String = King.printPiece(k)
  }

  private implicit val pawnMove = new PieceMovement[Pawn] {
    def movement(p: Pawn, c: Chessboard): List[Square] = Pawn.movement(p,c)

    def printPiece(p: Pawn): String = Pawn.printPiece(p)
  }

  private implicit val bishopMove = new PieceMovement[Bishop] {
    def movement(b: Bishop, c: Chessboard): List[Square] = Bishop.movement(b,c)

    def printPiece(b: Bishop): String = Bishop.printPiece(b)
  }

  private implicit val knightMove = new PieceMovement[Knight] {
    def movement(k: Knight, c: Chessboard): List[Square] = Knight.movement(k,c)

    def printPiece(k: Knight): String = Knight.printPiece(k)
  }

  private implicit val queenMove = new PieceMovement[Queen] {
    def movement(q: Queen, c: Chessboard): List[Square] = Queen.movement(q,c)

    def printPiece(q: Queen): String = Queen.printPiece(q)
  }

  private implicit val rookMove = new PieceMovement[Rook] {
    def movement(r: Rook, c: Chessboard): List[Square] = Rook.movement(r,c)

    def printPiece(r: Rook): String = Rook.printPiece(r)
  }
}