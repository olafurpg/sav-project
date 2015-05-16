package go.core

import go.core.CellObject.EmptyCell

object RuleEngine {
  def next(game: Game, step: Step): Either[Game, MoveError] = {
    import game._

    step match {
      case Pass => Left(Game(state :: states, step :: steps))

      case Place(x, y) if !state.insideBoard(Point(x, y)) => Right(OutsideOfBoardError)

      case Place(x, y) if state.isOccupied(Point(x, y)) => Right(AlreadyOccupiedError)

      case Place(x, y) if round > 1 && CaptureLogic.put(state, Point(x, y), activePlayer.cell) == states(1) => Right(AlreadyOccupiedError)

      case p @ Place(x, y) =>
        val newPoint = Point(x, y)
        val newBoard = CaptureLogic.put(state, newPoint, activePlayer.cell)
        if (newBoard.at(newPoint) == EmptyCell) Right(SuicideError)
        else if (round > 0 && newBoard == states.tail.head) Right(KoError)
        else Left(Game(newBoard :: states, step :: steps))
    }
  } ensuring { res =>
    res match {
      case Left(_) => !Step.isValid(step, game.state)
      case Right(_) => true
    }
  }

  def check(game: Game, step: Step): Option[MoveError] = next(game, step) match {
    case Left(g) => None
    case Right(e) => Some(e)
  }

  def isValid(game: Game, step: Step): Boolean = check(game, step).isEmpty
}