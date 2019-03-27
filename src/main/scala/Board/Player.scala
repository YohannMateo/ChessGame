package Board

import Board.Color.ColorVal

case class Player (color: ColorVal)

object Player {
  def printPlayer(p : Player) {
    println(p.color)
  }
}