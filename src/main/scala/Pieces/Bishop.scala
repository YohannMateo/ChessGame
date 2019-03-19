package Pieces

import Board.{Player, Square}

class Bishop (val p: Player, val s: Square, val c: String) extends Piece(p,s,c) {
  override val name = "Bishop"
}
