package player

import board.Board

trait Player {
  val playerBoard: Board
  val opponentBoard: Board
  val sunkenBoats: Int
}
