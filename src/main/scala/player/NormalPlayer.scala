package player

import scala.collection.mutable.ArrayBuffer
import board.{Board, Field}

class NormalPlayer(override val playerBoard: Board, override val opponentBoard: Board) extends Player(playerBoard, opponentBoard) {
  val fieldsShot: ArrayBuffer[Field] = ArrayBuffer.empty
}
