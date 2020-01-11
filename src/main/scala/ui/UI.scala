package ui

import java.lang.System.lineSeparator
import scala.io.StdIn.readLine

import board._
import game.Game.player
import exception.{InvalidFieldException, InvalidInputException}


object UI {

  def showBoard(board: Board): Unit = println(board + lineSeparator())

  def showBoard(board: Board, msg: String): Unit = println(msg + lineSeparator() + board + lineSeparator())

  def getField: Field = {
    val field = readLine
    Field.fromStringToField(field)
  }

  def getFieldToShootFromPlayer(msg: String): Field = {
    var field = Field(0,0)
    while (!field.isValid || player.fieldsShot.contains(field)) {
      println(msg)
      try {
        field = getField
        if (player.fieldsShot.contains(field)) println(Message.SHOOT_TO_THE_ALREADY_SHOT_FIELD)
      }
      catch {
        case _: InvalidInputException => println(Message.WRONG_FORMAT)
        case _: InvalidFieldException => println(Message.FIELD_OUT_OF_BOARD)
        case _: Throwable => println(Message.OTHER_KIND_OF_EXCEPTION)
      }
    }
    field
  }

  @scala.annotation.tailrec
  def addShipFromInput(board: Board, ship: Ship): Unit = {
    def showPossibleFields(field: Field, ship: Ship): Unit = {
      val possibilities = board.possibleFields(field, ship)
      println(s"All possible fields: ${possibilities.mkString(", ")}")
    }

    def getValidField(msg: String): Field = {
      var newField = Field(0, 0)
      while (!newField.isValid) {
        println(msg)
        try {
          newField = getField
          if (!board.fieldNotAdjoin(newField)) {
            println(Message.FIELD_ADJOINS_OTHER_SHIP)
            newField = getValidField(msg)
          }
        }
        catch {
          case _: InvalidInputException => println(Message.WRONG_FORMAT)
          case _: InvalidFieldException => println(Message.FIELD_OUT_OF_BOARD)
          case _: Throwable => println(Message.OTHER_KIND_OF_EXCEPTION)
        }
      }
      newField
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
