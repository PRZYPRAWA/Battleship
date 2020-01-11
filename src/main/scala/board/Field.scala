package board

import exception.{InvalidFieldException, InvalidInputException}

case class Field(col: Int, row: Int) {
  override def toString: String = (col + 'a' - 1).toChar.toString + row

  def relative(c: Int, r: Int) = Field(col + c, row + r)

  def isValid: Boolean = col >= 1 && col <= Board.boardSize && row >= 1 && row <= Board.boardSize

  def <(other: Field): Boolean = (col == other.col && row < other.row) || (row == other.row && col < other.col)
}

object Field {
  def fromStringToField(field: String): Field = {
    if (field.matches("[a-j]\\d{1,2}")) {
      val col = field.charAt(0) - 96 // because 'a' in ascii is 97
      val row = field.substring(1).toInt
      val newField = Field(col, row)

      if (newField.isValid) newField
      else throw new InvalidFieldException
    }
    else throw new InvalidInputException
  }
}
