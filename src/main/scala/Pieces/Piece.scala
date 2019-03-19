package Pieces

import Board.{Player, Square}

class Piece (val player: Player, var square: Square, val color: String) {
  val name = "Empty"
  def print () = {
    "  Piece : " + name + " - " + color
  }
}
