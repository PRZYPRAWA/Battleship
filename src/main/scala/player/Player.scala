package player

import board.Board

abstract class Player(val playerBoard: Board, val opponentBoard: Board) {
  var sunkenBoats = 0

  override def toString: String = "PLAYER"
}
