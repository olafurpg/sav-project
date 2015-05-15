package go.core

abstract class MoveError

case object KoError extends MoveError
case object OutsideOfBoardError extends MoveError
case object AlreadyOccupiedError extends MoveError
case object SuicideError extends MoveError

sealed abstract class Rule {
  def check(game: Game, step: Step): Option[MoveError]
}
