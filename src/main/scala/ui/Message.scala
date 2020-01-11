package ui

import board.Ship

object Message {

  val PLAYER_BOARD = "YOUR BOARD"
  val OPPONENT_BOARD = "OPPONENT'S BOARD"

  val WRITE_FIELD_COORDINATES: (String => String) = (position: String) => s"Write $position field coordinates (e.g. 'a1'): "
  val ADD_SHIP_TO_BOARD: (Ship => String) = (ship: Ship) => s"Add $ship ship to your board: length ${ship.len}"
  val WRONG_FORMAT = "Wrong format of the input, should be e.g.: 'a1'"
  val FIELD_OUT_OF_BOARD = "Field is not on the board"
  val FIELD_ADJOINS_OTHER_SHIP = "Field adjoins other ship, try again!"
  val WRONG_LENGTH_OR_ADJACENT_SHIP = "Wrong length or ship adjacent, try again!"

  val WRITE_FIELD_TO_SHOOT = "Write field to shoot coordinates (e.g. 'a1'): "
  val SHOOT_TO_THE_ALREADY_SHOT_FIELD = "You shot this field earlier, try again!"

  val _WON_THE_GAME: (String => String ) = (player: String) => s"$player WON THE GAME"

  val OTHER_KIND_OF_EXCEPTION = "Other kind of exception!"

}
