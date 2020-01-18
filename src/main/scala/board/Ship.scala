package board

sealed trait Ship {
  val len: Int
}

case class Carrier(len: Int = ShipConsts.carrierLen) extends Ship {
  override def toString: String = "C"
}

case class Battleship(len: Int = ShipConsts.battleshipLen) extends Ship {
  override def toString: String = "B"
}

case class Destroyer(len: Int = ShipConsts.destroyerLen) extends Ship {
  override def toString: String = "D"
}

case class Submarine(len: Int = ShipConsts.submarineLen) extends Ship {
  override def toString: String = "S"
}

case class PatrolBoat(len: Int = ShipConsts.patrolBoatLen) extends Ship {
  override def toString: String = "P"
}

case object Miss extends Ship {
  override val len: Int = 0
  override def toString: String = "o"
}

case object Shot extends Ship {
  override val len: Int = 0
  override def toString: String = "x"
}

object ShipConsts {
  val carrierLen = 5
  val battleshipLen = 4
  val destroyerLen = 3
  val submarineLen = 3
  val patrolBoatLen = 2

  val winningPoints = carrierLen + battleshipLen + destroyerLen + submarineLen + patrolBoatLen
}