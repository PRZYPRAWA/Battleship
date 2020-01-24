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
        case Nil =>
          ai
        case h :: t =>
          addShipFromList(ai.addShip(h), t)
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
        case Nil =>
          playerBoard
        case h :: t =>
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

  def playerShooting(player: NormalPlayer, ai: AI): ((NormalPlayer, AI), Boolean) = {
    val playerFieldToShoot = getFieldToShootFromPlayer(Message.WRITE_FIELD_TO_SHOOT)

    if (player.shotFields.contains(playerFieldToShoot)) {
      println(Message.SHOOTS_TO_THE_ALREADY_SHOT_FIELD)
      playerShooting(player, ai)
    }
    else {
      val (aiBoardAfterShot, replyAfterShot) = ai.playerBoard.shoot(playerFieldToShoot)
      val playerOpponentBoardAfterShot = player.opponentBoard.afterShot(playerFieldToShoot, replyAfterShot)

      println(s"$player $replyAfterShot")

      val didHit = replyAfterShot == Hit

      val playerToReturn = player.copy(
        opponentBoard = playerOpponentBoardAfterShot,
        shotFields = player.shotFields :+ playerFieldToShoot,
        sunkenBoats = player.sunkenBoats + {
          if (didHit) 1 else 0
        }
      )

      val aiToReturn = ai.copy(playerBoard = aiBoardAfterShot)

      ((playerToReturn, aiToReturn), didHit)
    }
  }

  def aiShooting(player: NormalPlayer, ai: AI): ((NormalPlayer, AI), Boolean) = {
    val aiFieldToShoot = ai.nextFieldToShoot.get

    val (playerBoardAfterShot, replyAfterShot) = player.playerBoard.shoot(aiFieldToShoot)
    val aiOpponentBoardAfterShot = ai.opponentBoard.afterShot(aiFieldToShoot, replyAfterShot)

    println(s"$ai $replyAfterShot")

    val didHit = replyAfterShot == Hit

    val playerToReturn = player.copy(playerBoard = playerBoardAfterShot)

    val aiToReturn = ai.copy(
      opponentBoard = aiOpponentBoardAfterShot,
      shotFields = ai.shotFields :+ aiFieldToShoot,
      sunkenBoats = player.sunkenBoats + {
        if (didHit) 1 else 0
      }
    ).setNextFieldAndDirection(aiFieldToShoot, didHit)

    ((playerToReturn, aiToReturn), didHit)
  }

  def playerTurn(player: NormalPlayer, ai: AI): Player = {
    val ((playerAfterShot, aiAfterShot), didHit) = playerShooting(player, ai)
    update(playerAfterShot.playerBoard, playerAfterShot.opponentBoard)
    Thread.sleep(2000)

    if (playerAfterShot.sunkenBoats == ShipConstants.winningPoints) playerAfterShot
    else if (didHit) playerTurn(playerAfterShot, aiAfterShot)
    else aiTurn(playerAfterShot, aiAfterShot)
  }

  def aiTurn(player: NormalPlayer, ai: AI): Player = {
    val ((playerAfterShot, aiAfterShot), didHit) = aiShooting(player, ai)
    update(playerAfterShot.playerBoard, playerAfterShot.opponentBoard)
    Thread.sleep(2000)

    if (aiAfterShot.sunkenBoats == ShipConstants.winningPoints) aiAfterShot
    else if (didHit) aiTurn(playerAfterShot, aiAfterShot)
    else playerTurn(playerAfterShot, aiAfterShot)
  }

  def startGame(): Unit = {
    val ai = addAiShips().setNextFieldAndDirection(Field(0, 0), false)
    showBoard(ai.playerBoard)

    val player = addPlayerShips()
    showBoard(player.playerBoard)

    val r = scala.util.Random.nextDouble() // random number between 0 and 1 to decide who is starting
    val winner = if (r > 0.5) {
      println(Message.IS_STARTING_THE_GAME(player))
      Thread.sleep(2000)

      playerTurn(player, ai)
    } else {
      println(Message.IS_STARTING_THE_GAME(ai))
      Thread.sleep(2000)

      aiTurn(player, ai)
    }

    println(Message._WON_THE_GAME(winner))
  }

}
