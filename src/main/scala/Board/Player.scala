package Board

import Board.Color.Color

case class Player (color: Color)

object Player {
  def printPlayer(p : Player) {
    println(p.color)
  }
}