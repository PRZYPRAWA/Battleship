package board

import java.lang.System.lineSeparator
import scala.collection.mutable.ArrayBuffer

object Board {
  val boardSize = 10
}

class Board {
  var board: Map[Field, Ship] = Map.empty

  def shoot(field: Field): Reply = {
    if (field.isValid) {
      val attempt: Ship = board.getOrElse(field, Miss)
      if (attempt == Miss) {
        board += (field -> Miss)
        Mishit
      }
      else {
        attempt.hit()
        board += (field -> Shot)
        if (attempt.len == 0) Sunk
        else Hit
      }
    }
    else Mishit
  }

  def afterShot(field: Field, reply: Reply): Unit = {
    if (field.isValid) {
      if (reply == Mishit) board += (field -> Miss)
      else board += (field -> Shot)
    }
  }

  def fieldNotAdjoin(field: Field): Boolean = {
    !board.contains(field) &&
      !board.contains(field.relative(0, 1)) &&
      !board.contains(field.relative(0, -1)) &&
      !board.contains(field.relative(1, 0)) &&
      !board.contains(field.relative(1, 1)) &&
      !board.contains(field.relative(1, -1)) &&
      !board.contains(field.relative(-1, 0)) &&
      !board.contains(field.relative(-1, 1)) &&
      !board.contains(field.relative(-1, -1))
  }

  def shipNotAdjoin(begin: Field, end: Field, vertical: Boolean): Boolean = {
    @scala.annotation.tailrec
    def checkDoesNotAdjoin(begin: Field, end: Field, vertical: Boolean): Boolean = {
      if (begin == end) !board.contains(begin)
      else if (vertical) {
        !board.contains(begin) && checkDoesNotAdjoin(begin.relative(0, 1), end, vertical)
      } else {
        !board.contains(begin) && checkDoesNotAdjoin(begin.relative(1, 0), end, vertical)
      }
    }

    val (beginField, endField) = if (begin < end) (begin, end) else (end, begin)
    if (vertical) {
      checkDoesNotAdjoin(beginField.relative(0, -1), endField.relative(0, 1), true) &&
        checkDoesNotAdjoin(beginField.relative(-1, -1), endField.relative(-1, 1), true) &&
        checkDoesNotAdjoin(beginField.relative(1, -1), endField.relative(1, 1), true)
    }
    else {
      checkDoesNotAdjoin(beginField.relative(-1, 0), endField.relative(1, 0), false) &&
        checkDoesNotAdjoin(beginField.relative(-1, -1), endField.relative(1, -1), false) &&
        checkDoesNotAdjoin(beginField.relative(-1, 1), endField.relative(1, 1), false)
    }
  }

  def addShip(beginField: Field, endField: Field, ship: Ship): Boolean = {
    @scala.annotation.tailrec
    def add(begin: Field, end: Field, ship: Ship, vertical: Boolean): Unit = {
      if (begin == end) board += (begin -> ship)
      else if (vertical) {
        board += (begin -> ship)
        add(begin.relative(0, 1), end, ship, vertical)
      } else {
        board += (begin -> ship)
        add(begin.relative(1, 0), end, ship, vertical)
      }
    }

    if (beginField.isValid
      && endField.isValid
      && beginField.col == endField.col
      && shipNotAdjoin(beginField, endField, true)
      && (Math.abs(endField.row - beginField.row) + 1 == ship.len)) {

      if (beginField < endField) add(beginField, endField, ship, true)
      else add(endField, beginField, ship, true)
      true
    }
    else if (beginField.isValid
      && endField.isValid
      && beginField.row == endField.row
      && shipNotAdjoin(beginField, endField, false)
      && (Math.abs(endField.col - beginField.col) + 1 == ship.len)) {

      if (beginField < endField) add(beginField, endField, ship, false)
      else add(endField, beginField, ship, false)
      true
    }
    else false
  }

  def possibleFields(field: Field, ship: Ship): ArrayBuffer[Field] = {
    var possibilities: ArrayBuffer[Field] = ArrayBuffer.empty
    val shipLength = ship.len - 1

    var fstPossible = field.relative(0, shipLength)
    if (fstPossible.isValid
      && shipNotAdjoin(field, fstPossible, true))
      possibilities += fstPossible

    var sndPossible = field.relative(0, -shipLength)
    if (sndPossible.isValid
      && shipNotAdjoin(field, sndPossible, true))
      possibilities += sndPossible

    var thirdPossible = field.relative(shipLength, 0)
    if (thirdPossible.isValid
      && shipNotAdjoin(field, thirdPossible, false))
      possibilities += thirdPossible

    var fourthPossible = field.relative(-shipLength, 0)
    if (fourthPossible.isValid
      && shipNotAdjoin(field, fourthPossible, false))
      possibilities += fourthPossible

    possibilities
  }

  override def toString: String = {
    def rowToString(row: Int) = 1.to(Board.boardSize).map(col =>
      board.get(Field(col, row)).map(" " + _.toString).getOrElse(" -")).mkString

    " " + " abcdefghij".mkString(" ") + lineSeparator() + 1.to(Board.boardSize).map {
      case Board.boardSize => Board.boardSize.toString + rowToString(Board.boardSize) + " " + Board.boardSize.toString + lineSeparator()
      case row => row.toString + " " + rowToString(row) + " " + row.toString + lineSeparator()
    }.mkString + " " + " abcdefghij".mkString(" ")
  }

}
