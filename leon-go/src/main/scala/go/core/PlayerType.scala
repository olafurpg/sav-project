package go.core
import CellObject._

sealed abstract class PlayerType {
  def cell: Cell = this match {
    case WhitePlayer => WhiteCell
    case BlackPlayer => BlackCell
  }
  def nextPlayer: PlayerType = this match {
    case WhitePlayer => BlackPlayer
    case BlackPlayer => WhitePlayer
  }
  def previousPlayer: PlayerType = nextPlayer
}

case object WhitePlayer extends PlayerType
case object BlackPlayer extends PlayerType
