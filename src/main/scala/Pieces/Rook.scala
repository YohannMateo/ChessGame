package Pieces

import Board.{Player, Square}

class Rook (val p: Player, var s: Square, val c: String) extends Piece(p,s,c) {
  override val name = "Rook"
}
