package game

import board._
import player._
import ui.Message
import ui.UI._

object Game {

  def update(playerBoard: Board, opponentBoard: Board): Unit = {
    showBoard(playerBoard, Message.PLAYER_BOARD)
    showBoard(opponentBoard, Message.OPPONENT_BOARD)
  }

  def addAiShips(): AI = {
    def addShipFromList(ai: AI, ships: List[Ship]): AI = {
      ships match {
        case Nil => ai
        case h::t => addShipFromList(ai.addShip(h), t)
      }
    }

    val ships = List(
      Carrier(),
      Battleship(),
      Destroyer(),
      Submarine(),
      PatrolBoat()
    )

    val ai = AI(Board(), Board())
    addShipFromList(ai, ships)
  }

  def addPlayerShips(): NormalPlayer = {
    def addShipFromList(playerBoard: Board, ships: List[Ship]): Board = {
      ships match {
        case Nil => playerBoard
        case h::t =>
          showBoard(playerBoard, Message.PLAYER_BOARD)
          addShipFromList(addShipFromInput(playerBoard, h), t)
      }
    }

    val ships = List(
      Carrier(),
      Battleship(),
      Destroyer(),
      Submarine(),
      PatrolBoat()
    )

    val playerBoard = addShipFromList(Board(), ships)
    NormalPlayer(playerBoard, Board())
  }

  def startGame(): Unit = {
    val ai = addAiShips()
    showBoard(ai.playerBoard)

    val player = addPlayerShips()
    showBoard(player.playerBoard)
  }

}
