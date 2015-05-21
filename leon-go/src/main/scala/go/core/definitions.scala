package go.core

object definitions {

  // a stone
  sealed abstract class Cell {
    def otherColor: Cell = {
      this match {
        case WhiteCell => BlackCell
        case BlackCell => WhiteCell
        case EmptyCell => EmptyCell
      }
    } ensuring (res => res == EmptyCell || (res != this))
  }

  case object WhiteCell extends Cell
  case object BlackCell extends Cell
  case object EmptyCell extends Cell

  // point on board
  case class Point(x: BigInt, y: BigInt)
  case class PlacedCell(p: Point, c: Cell)

  // a game step
  sealed abstract class Step
  case object Pass extends Step
  case class Place(x: BigInt, y: BigInt) extends Step

  // game move error
  abstract class MoveError

  case object KoError extends MoveError
  case object OutsideOfBoardError extends MoveError
  case object AlreadyOccupiedError extends MoveError
  case object SuicideError extends MoveError

  // player type
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
