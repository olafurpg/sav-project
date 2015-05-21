package go.core

import go.core.definitions._
import leon.annotation._
import go.collection._

object RuleEngine {
  private def nextPlayer(game: Game): PlayerType = {
    game.activePlayer match {
      case WhitePlayer => BlackPlayer
      case BlackPlayer => WhitePlayer
    }
  } ensuring(_ != game.activePlayer)

  def next(game: Game, step: Step): GoEither[Game, MoveError] = {
    require(game.isValid)

    step match {
      case Pass =>
        GoLeft[Game, MoveError](Game(game.states, Pass::game.steps, nextPlayer(game)))

      case Place(x, y) if !game.state.insideBoard(Point(x, y)) =>
        GoRight[Game, MoveError](OutsideOfBoardError)

      case Place(x, y) if game.state.isOccupied(Point(x, y)) =>
        GoRight[Game, MoveError](AlreadyOccupiedError)

      case p @ Place(x, y) =>
        val newPoint = Point(x, y)
        val newBoard = CaptureLogic.capture(game.state, newPoint, game.activePlayer.cell)

        if (newBoard.at(newPoint) == EmptyCell)
          GoRight[Game, MoveError](SuicideError)
        else if (game.round > 0 && newBoard.isEqual(game.states.tail.head))
          GoRight[Game, MoveError](KoError)
        else
          GoLeft[Game, MoveError](Game(newBoard :: game.states, step :: game.steps, nextPlayer(game)))
    }
  } ensuring { res =>
    res match {
      case GoLeft(newGame) =>
        step match {
          case Pass => newGame.activePlayer == nextPlayer(game)
          case Place(x, y) => !game.state.isOccupied(Point(x, y))
        }
      case GoRight(err) => true
    }
  }

  @ignore
  def check(game: Game, step: Step): Option[MoveError] = next(game, step) match {
    case GoLeft(g) => None
    case GoRight(e) => Some(e)
  }

  @ignore
  def isValid(game: Game, step: Step): Boolean = check(game, step).isEmpty

  def isOver(game: Game): Boolean = {
    require(game.isValid)
    game.round > 1 && game.steps.head == Pass && game.steps.tail.head == Pass
  }

  @ignore
  /** Implements area scoring (VS territory scoring)
    *
    * A player's score is the number of stones that the player has
    * on the board, plus the number of empty intersections surrounded
    * by that player's stones
    */
  def score(game: Game): Map[PlayerType, Int] = {
    ???
  }
}
