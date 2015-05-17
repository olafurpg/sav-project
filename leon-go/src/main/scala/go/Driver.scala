package go

import go.core.PlayerTypeObject._
import go.core._
import go.player._
import leon.annotation._

object Driver {
  @ignore
  def main(args: Array[String]): Unit = {
    run(Game(5), Map(WhitePlayer -> HumanPlayer, BlackPlayer -> ComputerPlayer))
  }

  @ignore
  def run(game: Game, players: Map[PlayerType, Player]): Unit = {
    if (!RuleEngine.isOver(game)) {
      RuleEngine.next(game, players(game.activePlayer).move(game)) match {
        case Left(game) => run(game, players)
        case Right(err) => println(err); run(game, players)
      }
    }
  }
}
