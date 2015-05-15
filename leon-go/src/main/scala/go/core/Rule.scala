package go.core

import go.core.CellObject.EmptyCell

object Rule {
  def check(game: Game, step: Step): Option[MoveError] = {
    import game._
    step match {
      case Pass => None

      case Place(x, y) if !state.insideBoard(Point(x, y)) => Some(OutsideOfBoardError)

      case Place(x, y) if state.isOccupied(Point(x, y)) => Some(AlreadyOccupiedError)

      case Place(x, y) if round > 1 && CaptureLogic.put(state, Point(x, y), activePlayer.cell) == states(1) => Some(AlreadyOccupiedError)

      case p @ Place(x, y) =>
        val newPoint = Point(x, y)
        val newBoard = CaptureLogic.put(state, newPoint, activePlayer.cell)
        if (newBoard.at(newPoint) == EmptyCell) Some(SuicideError)
        else if (round > 0 && newBoard == states.tail.head) Some(KoError)
        else None
    }
  } ensuring {
    _ match {
      case Some(AlreadyOccupiedError) => !Step.isValid(step, game.state)
      case None => true
      case _ => true
    }
  }
}

