package sav.go.leon

trait GameRules {
  def move(g: Game, s: Step): Either[Game, MoveError]
}

//case object ChineseRules extends GameRules

trait MoveError

case object KoError extends MoveError
case object OutsideOfBoardError extends MoveError
case object AlreadyOccupiedError extends MoveError
case object SuicideError extends MoveError


