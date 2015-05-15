package go.core

// TODO: rename to stone
object CellObject {

  sealed abstract class Cell {
    def otherColor: Cell = this match {
      case WhiteCell => BlackCell
      case BlackCell => WhiteCell
      case EmptyCell => EmptyCell
    }
  }

  case object WhiteCell extends Cell
  case object BlackCell extends Cell
  case object EmptyCell extends Cell
}

