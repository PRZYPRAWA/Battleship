package player

import scala.collection.mutable.ArrayBuffer
import board.{Board, Field, Ship}

class AI(override val playerBoard: Board, override val opponentBoard: Board) extends Player(playerBoard, opponentBoard) {

  sealed trait Direction

  case object Up extends Direction

  case object Down extends Direction

  case object Left extends Direction

  case object Right extends Direction

  override def toString: String = "AI"

  val directions: ArrayBuffer[Direction] = ArrayBuffer(Up, Down, Left, Right)

  val shots: ArrayBuffer[Field] = ArrayBuffer.empty

  var nextField: Option[Field] = None
  var nextDirection: Option[Direction] = Some(Down)

  @scala.annotation.tailrec
  private def randomValidField: Field = {
    val r = new scala.util.Random
    val field = Field(r.nextInt(Board.boardSize) + 1, r.nextInt(Board.boardSize) + 1)
    if (field.isValid) field
    else randomValidField
  }

  private def randomFieldToAddShip: Field = {
    val field = randomValidField
    if (playerBoard.fieldNotAdjoin(field)) field
    else randomFieldToAddShip
  }

  private def randomFieldToShoot: Field = {
    val field = randomValidField
    if (shots.contains(field)) randomFieldToShoot
    else field
  }

  def choose[A](array: ArrayBuffer[A]): A = {
    val r = new scala.util.Random
    array(r.nextInt(array.length))
  }

  def addShip(ship: Ship): Unit = {
    val beginField = randomFieldToAddShip

    val possibleEnd = playerBoard.possibleFields(beginField, ship)
    val endField = choose(possibleEnd)
    playerBoard.addShip(beginField, endField, ship)
  }

  def setNextFieldAndDirection(field: Field, didHit: Boolean): Unit = {
    if (!didHit) {
      nextDirection = None
      nextField = None
    } else {
      nextField = nextDirection match {
        case Some(Up) => Some(field.relative(0, -1))
        case Some(Down) => Some(field.relative(0, 1))
        case Some(Left) => Some(field.relative(-1, 0))
        case Some(Right) => Some(field.relative(1, 0))
        case None => {
          nextDirection = Some(choose(directions))
          nextDirection match {
            case Some(Up) => Some(field.relative(0, -1))
            case Some(Down) => Some(field.relative(0, 1))
            case Some(Left) => Some(field.relative(-1, 0))
            case Some(Right) => Some(field.relative(1, 0))
          }
        }
      }
    }
  }

  def fieldToShoot: Field = {
    if (nextField == None) {
      val field = randomFieldToShoot

      if (shots.contains(field)) fieldToShoot
      else {
        shots += field

        field
      }
    } else {
      val field = nextField.get
      if (field.isValid && !shots.contains(field)) {
        shots += field

        field
      }
      else randomFieldToShoot
    }
  }

}
