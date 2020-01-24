package board

import java.lang.System.lineSeparator

object Board {
  val boardSize = 10

  def apply(board: Map[Field, Ship] = Map.empty) = new Board(board)
}

class Board(val board: Map[Field, Ship] = Map()) {

  def shoot(field: Field): (Board, Reply) = {
    if (field.isValid) {
      val attempt = board.getOrElse(field, Miss)
      if (attempt == Miss) (Board(board + (field -> Miss)), Mishit)
      else (Board(board + (field -> Shot)), Hit)
    }
    else (Board(board), Mishit)
  }

  def afterShot(field: Field, reply: Reply): Board = {
    if (field.isValid) {
      reply match {
        case Mishit => new Board(board + (field -> Miss))
        case Hit => new Board(board + (field -> Shot))
      }
    }
    else Board(board)
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
      if (begin == end) fieldNotAdjoin(begin)
      else if (vertical) {
        fieldNotAdjoin(begin) && checkDoesNotAdjoin(begin.relative(0, 1), end, vertical)
      } else {
        fieldNotAdjoin(begin) && checkDoesNotAdjoin(begin.relative(1, 0), end, vertical)
      }
    }

    val (beginField, endField) = if (begin < end) (begin, end) else (end, begin)
    if (vertical) {
      //      checkDoesNotAdjoin(beginField.relative(0, -1), endField.relative(0, 1), true) &&
      //        checkDoesNotAdjoin(beginField.relative(-1, -1), endField.relative(-1, 1), true) &&
      //        checkDoesNotAdjoin(beginField.relative(1, -1), endField.relative(1, 1), true)
      checkDoesNotAdjoin(beginField, endField, true)
    }
    else {
      //      checkDoesNotAdjoin(beginField.relative(-1, 0), endField.relative(1, 0), false) &&
      //        checkDoesNotAdjoin(beginField.relative(-1, -1), endField.relative(1, -1), false) &&
      //        checkDoesNotAdjoin(beginField.relative(-1, 1), endField.relative(1, 1), false)
      checkDoesNotAdjoin(beginField, endField, false)
    }
  }

  def addShip(beginField: Field, endField: Field, ship: Ship): Board = {

    def add(begin: Field, end: Field, ship: Ship, vertical: Boolean): Map[Field, Ship] = {
      if (begin == end) Map(begin -> ship)
      else if (vertical) {
        add(begin.relative(0, 1), end, ship, vertical) + (begin -> ship)
      } else {
        add(begin.relative(1, 0), end, ship, vertical) + (begin -> ship)
      }
    }

    if (beginField.isValid
      && endField.isValid
      && beginField.col == endField.col
      && beginField.row != endField.row
      && shipNotAdjoin(beginField, endField, true)
      && (Math.abs(endField.row - beginField.row) + 1 == ship.len)) {

      if (beginField < endField) new Board(board ++ add(beginField, endField, ship, true))
      else new Board(board ++ add(endField, beginField, ship, true))
    }
    else if (beginField.isValid
      && endField.isValid
      && beginField.row == endField.row
      && beginField.col != endField.col
      && shipNotAdjoin(beginField, endField, false)
      && (Math.abs(endField.col - beginField.col) + 1 == ship.len)) {

      if (beginField < endField) new Board(board ++ add(beginField, endField, ship, false))
      else new Board(board ++ add(endField, beginField, ship, false))
    }
    else Board(board)
  }

  def possibleFields(field: Field, ship: Ship): Array[Field] = {
    val shipLength = ship.len - 1

    val horizontalPossibilities =
      Array(
        field.relative(shipLength, 0),
        field.relative(-shipLength, 0)
      )

    val verticalPossibilities = Array(
      field.relative(0, shipLength),
      field.relative(0, -shipLength)
    )

    verticalPossibilities.filter(elem => elem.isValid && shipNotAdjoin(field, elem, true)) ++
      horizontalPossibilities.filter(elem => elem.isValid && shipNotAdjoin(field, elem, false))
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
