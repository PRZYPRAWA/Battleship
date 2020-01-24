package player

import board.{Board, Field}

case class NormalPlayer(playerBoard: Board,
                        opponentBoard: Board,
                        sunkenBoats: Int = 0,
                        shotFields: Array[Field] = Array.empty
                       ) extends Player {
  override def toString: String = "PLAYER"
}
