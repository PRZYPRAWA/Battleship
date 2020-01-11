package board

sealed trait Reply

case object Hit extends Reply

case object Mishit extends Reply

case object Sunk extends Reply
