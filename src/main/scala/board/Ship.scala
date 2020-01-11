package board

sealed trait Ship {
  var len: Int
  def hit(): Unit = if (len == 0) () else len -= 1
}

class Carrier extends Ship {
  override var len: Int = 5
  override def toString: String = "C"
}

class Battleship extends Ship {
  override var len: Int = 4
  override def toString: String = "B"
}

class Destroyer extends Ship {
  override var len: Int = 3
  override def toString: String = "D"
}

class Submarine extends Ship {
  override var len: Int = 3
  override def toString: String = "S"
}

class PatrolBoat extends Ship {
  override var len: Int = 2
  override def toString: String = "P"
}

case object Miss extends Ship {
  override var len: Int = 0
  override def toString: String = "o"
}

case object Shot extends Ship {
  override var len: Int = 0
  override def toString: String = "x"
}
