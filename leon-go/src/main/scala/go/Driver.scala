package go

import go.core.definitions._
import go.core._
import go.player._
import leon.annotation._
import go.collection._

object Driver {
  @ignore
  def main(args: Array[String]): Unit = {
    run(Game(5), Map(BlackPlayer -> HumanPlayer,  WhitePlayer -> ComputerPlayer))
  }

  @ignore
  def run(game: Game, players: Map[PlayerType, Player]): Unit = {
    if (!RuleEngine.isOver(game)) {
      RuleEngine.next(game, players(game.activePlayer).move(game)) match {
        case GoLeft(game) => run(game, players)
        case GoRight(err) => println(err); run(game, players)
      }
    }
  }
}
