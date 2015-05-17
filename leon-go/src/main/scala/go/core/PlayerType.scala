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
  }

  case object WhitePlayer extends PlayerType
  case object BlackPlayer extends PlayerType
}
