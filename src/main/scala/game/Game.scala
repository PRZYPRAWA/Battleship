package game

import player._
import ui.UI._
import ui.Message
import board._

object Game {
  val ai = new AI(new Board, new Board)
  val player = new NormalPlayer(new Board, new Board)

  def update(): Unit = {
    showBoard(player.playerBoard, Message.PLAYER_BOARD)
    showBoard(player.opponentBoard, Message.OPPONENT_BOARD)
  }

  def replyAfterShooting(playerShooting: Player, boardBeingShot: Board, field: Field): Boolean = {
    val reply = boardBeingShot.shoot(field)

    playerShooting.opponentBoard.afterShot(field, reply)
    if (reply == Sunk) playerShooting.sunkenBoats += 1
    if (playerShooting == player) println(reply.toString.toUpperCase() + "!!!")

    reply != Mishit
  }

  def playerShooting(msg: String): Boolean = {
    val field = getFieldToShootFromPlayer(msg)
    player.fieldsShot += field

    replyAfterShooting(player, ai.playerBoard, field)
  }

  def aiShooting(): Boolean = {
    val field = ai.fieldToShoot

    val didHit = replyAfterShooting(ai, player.playerBoard, field)

    ai.setNextFieldAndDirection(field, didHit)

    didHit
  }

  def startGame(): Unit = {
    val aiShips = List(new Carrier,
      new Battleship,
      new Destroyer,
      new Submarine,
      new PatrolBoat)

    aiShips.foreach(ship => ai.addShip(ship))

    val playerShips = List(new Carrier,
      new Battleship,
      new Destroyer,
      new Submarine,
      new PatrolBoat)

    showBoard(player.playerBoard, Message.PLAYER_BOARD)
    playerShips.foreach(ship => {
      addShipFromInput(player.playerBoard, ship)
      showBoard(player.playerBoard, Message.PLAYER_BOARD)
    })

    val winningPoints = 5
    do {

      while (player.sunkenBoats != winningPoints && playerShooting(Message.WRITE_FIELD_TO_SHOOT)) {
        update()
      }

      while (ai.sunkenBoats != winningPoints && player.sunkenBoats != winningPoints && aiShooting()) {
        println("AFTER AI SHOOTING")
        update()
      }
      update()

    } while (player.sunkenBoats != winningPoints && ai.sunkenBoats != winningPoints)

    val winner = if (player.sunkenBoats == winningPoints) player else ai
    println(Message._WON_THE_GAME(winner.toString))
  }

}
