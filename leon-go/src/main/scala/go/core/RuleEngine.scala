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
          case Pass => checkPass(newGame, game)
          case p@Place(x, y) => checkValidPlace(game, newGame, p)
        }
      case GoRight(err) =>
        step match {
          case Pass => false
          case p@Place(x, y) => checkErrorPlace(game, err, p)
        }
    }
  }

  private def checkPass(oldGame: Game, newGame: Game): Boolean = {
    newGame.activePlayer == nextPlayer(oldGame) &&
    newGame.states == oldGame.states &&
    newGame.steps == Pass::oldGame.steps
  }

  private def checkValidPlace(oldGame: Game, newGame: Game, p: Place): Boolean = {
    !oldGame.state.isOccupied(Point(p.x, p.y))
  }

  private def checkErrorPlace(game: Game, err: MoveError, p: Place): Boolean = {
    if (isOutsideBoard(game, p)) err == OutsideOfBoardError
    else if (isAlreadyOccupied(game, p)) err == AlreadyOccupiedError
    else true
  }

  private def isAlreadyOccupied(game: Game, p: Place): Boolean = {
    game.state.isOccupied(Point(p.x, p.y))
  }

  private def isOutsideBoard(game: Game, p: Place): Boolean = {
    !game.state.insideBoard(Point(p.x, p.y))
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
  /** Implements stone scoring
    *
    *  Stone scoring just counts the stones on board. It's much
    *  simpler than *territory scoring* and *area scoring*.
    */
  def score(game: Game): Map[PlayerType, Int] = {
    val zero = Map[PlayerType, Int](WhitePlayer -> 0, BlackPlayer -> 0)
    game.state.cells.foldLeft(zero) { (map, pair) =>
      pair._2 match {
        case WhiteCell => map + (WhitePlayer -> (map(WhitePlayer) + 1))
        case BlackCell => map + (BlackPlayer -> (map(BlackPlayer) + 1))
      }
    }
  }
}
