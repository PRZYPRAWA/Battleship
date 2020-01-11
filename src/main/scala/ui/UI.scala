package ui

import java.lang.System.lineSeparator
import scala.io.StdIn.readLine

import board._


object UI {

  def showBoard(board: Board): Unit = println(board + lineSeparator())

  def showBoard(board: Board, msg: String): Unit = println(msg + lineSeparator() + board + lineSeparator())

  def getField: (Option[Field], Option[String]) = {
    val field = readLine
    Field.fromStringToField(field)
  }

  def getFieldToShootFromPlayer(msg: String): Field = {
    println(msg)

    val (field, errorMessage) = getField

    (field, errorMessage) match {
      case (Some(field), None) => field
      case (None, Some(err)) =>
        println(err)
        getFieldToShootFromPlayer(msg)
      case _ => throw new Exception("Unexpected error!")
    }
  }

  @scala.annotation.tailrec
  def addShipFromInput(board: Board, ship: Ship): Unit = {
    def showPossibleFields(field: Field, ship: Ship): Unit = {
      val possibilities = board.possibleFields(field, ship)
      println(s"All possible fields: ${possibilities.mkString(", ")}")
    }

    def getValidField(msg: String): Field = {
      println(msg)

      val (field, errorMessage) = getField

      (field, errorMessage) match {
        case (Some(field), None) =>
          if (!board.fieldNotAdjoin(field)) {
            println(Message.FIELD_ADJOINS_OTHER_SHIP)
            getValidField(msg)
          }
          else field
        case (None, Some(err)) =>
          println(err)
          getFieldToShootFromPlayer(msg)
        case _ => throw new Exception("Unexpected error!")
      }
    }

    println(Message.ADD_SHIP_TO_BOARD(ship))

    val firstField = getValidField(Message.WRITE_FIELD_COORDINATES("first"))
    showPossibleFields(firstField, ship)

    val secondField = getValidField(Message.WRITE_FIELD_COORDINATES("second"))

    if (board.addShip(firstField, secondField, ship)) ()
    else {
      println(Message.WRONG_LENGTH_OR_ADJACENT_SHIP)
      addShipFromInput(board, ship)
    }
  }

}
