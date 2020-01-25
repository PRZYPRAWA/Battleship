package ui

import java.lang.System.lineSeparator
import scala.io.StdIn.readLine

import board._


object UI {
  def showBoard(board: Board, msg: String = ""): Unit = println(msg + lineSeparator() + board + lineSeparator())

  def getField: (Option[Field], Option[String]) = {
    val field = readLine
    Field.fromStringToField(field)
  }

  def getFieldToShootFromPlayer(msg: String): Field = {
    println(msg)

    val (field, errorMessage) = getField

    (field, errorMessage) match {
      case (Some(value), None) => value
      case (None, Some(err)) =>
        println(err)
        getFieldToShootFromPlayer(msg)
      case _ => throw new Exception("Unexpected error!")
    }
  }

  @scala.annotation.tailrec
  def addShipFromInput(board: Board, ship: Ship): Board = {

    def showPossibleFields(field: Field, ship: Ship): Unit = {
      val possibilities = board.possibleFields(field, ship)
      println(s"All possible fields: ${possibilities.mkString(", ")}")
    }

    def getValidField(msg: String): Field = {
      println(msg)

      val (field, errorMessage) = getField

      (field, errorMessage) match {
        case (Some(value), None) =>
          if (!board.fieldNotAdjoin(value)) {
            println(Message.FIELD_ADJOINS_OTHER_SHIP)
            getValidField(msg)
          }
          else value
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

    val newBoard = board.addShip(firstField, secondField, ship)
    if (newBoard.board == board.board) {
      println(Message.WRONG_LENGTH_OR_ADJACENT_SHIP)
      addShipFromInput(board, ship)
    }
    else {
      newBoard
    }
  }

}
