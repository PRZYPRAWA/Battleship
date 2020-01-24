package player

import board.{Board, Field, Ship}

case class AI(playerBoard: Board,
              opponentBoard: Board,
              nextFieldToShoot: Option[Field] = None,
              nextDirection: Option[Direction] = None,
              sunkenBoats: Int = 0,
              shotFields: Array[Field] = Array.empty
             ) extends Player {

  override def toString: String = "AI"

  val directions: Array[Direction] = Array(Up, Down, Left, Right)

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

  def randomFieldToShoot: Field = {
    val field = randomValidField
    if (shotFields.contains(field)) randomFieldToShoot
    else field
  }

  private def choose[A](array: Array[A]): A = {
    val r = new scala.util.Random
    array(r.nextInt(array.length))
  }

  def addShip(ship: Ship): AI = {
    val beginField = randomFieldToAddShip

    val possibleEnd = playerBoard.possibleFields(beginField, ship)
    val endField = choose(possibleEnd)

    AI(
      playerBoard.addShip(beginField, endField, ship),
      opponentBoard,
      nextFieldToShoot,
      nextDirection,
      sunkenBoats,
      shotFields
    )
  }

  def setNextFieldAndDirection(field: Field, didHit: Boolean): AI = {
    if (!didHit) {
      fieldToShoot
    } else {
      val newDirection = if (nextDirection.isEmpty) Some(choose(directions)) else nextDirection
      val newField = newDirection match {
        case Some(Up) => Some(field.relative(0, -1))
        case Some(Down) => Some(field.relative(0, 1))
        case Some(Left) => Some(field.relative(-1, 0))
        case Some(Right) => Some(field.relative(1, 0))
        case None => throw new Exception("Unexpected error: AI, setNextFieldAndDirection")
      }

      if (shotFields.contains(newField.get))
        setNextFieldAndDirection(newField.get, didHit)
      else
        AI(
          playerBoard,
          opponentBoard,
          newField,
          newDirection,
          sunkenBoats,
          shotFields :+ newField.get
        )
    }
  }

  private def fieldToShoot: AI = {
    if (nextFieldToShoot.isEmpty) {
      val field = randomFieldToShoot

      if (shotFields.contains(field)) fieldToShoot
      else {
        AI(
          playerBoard,
          opponentBoard,
          Some(field),
          nextDirection,
          sunkenBoats,
          shotFields :+ field
        )
      }
    } else {
      val field = nextFieldToShoot.get
      if (field.isValid && !shotFields.contains(field)) {
        AI(
          playerBoard,
          opponentBoard,
          Some(field),
          nextDirection,
          sunkenBoats,
          shotFields :+ field
        )
      } else {
        val randomField = randomFieldToShoot
        AI(
          playerBoard,
          opponentBoard,
          Some(randomField),
          nextDirection,
          sunkenBoats,
          shotFields :+ randomField
        )
      }
    }
  }
}
