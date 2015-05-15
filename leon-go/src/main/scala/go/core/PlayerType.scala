package go.core
import CellObject._

object PlayerTypeObject {

  sealed abstract class PlayerType {
    def cell: Cell = {
      this match {
        case WhitePlayer => WhiteCell
        case BlackPlayer => BlackCell
      }
    }
    def nextPlayer: PlayerType = {
      this match {
        case WhitePlayer => BlackPlayer
        case BlackPlayer => WhitePlayer
      }
    } ensuring(_ != this)
    def previousPlayer: PlayerType = nextPlayer
  }

  case object WhitePlayer extends PlayerType
  case object BlackPlayer extends PlayerType
}
