package go

import go.core.definitions._
import go.core._
import go.player._
import go.util.StringUtil
import leon.annotation._
import go.collection._


object Driver extends StringUtil {
  @ignore
  def main(args: Array[String]): Unit = {
    val game = Game(5)

    println("Welcome to Go!\n")
    println(boardToString(game.state))

    run(Game(5), Map(BlackPlayer -> HumanPlayer,  WhitePlayer -> RandomPlayer))
  }

  @ignore
  def run(game: Game, players: Map[PlayerType, Player]): Unit = {
    if (!RuleEngine.isOver(game)) {
      val player = players(game.activePlayer)
      val step = player.move(game)
      RuleEngine.next(game, step) match {
        case GoLeft(g) =>
          println(player.name + ": " + step + " Stone:" + cellToString(game.activePlayer.cell) + "\n")
          println(boardToString(g.state))
          run(g, players)
        case GoRight(err) => println(err); run(game, players)
      }
    }
    else {
      val score = RuleEngine.score(game)
      println("\n======== RESULT ==========\n")
      println(players(WhitePlayer).name + ":" + score(WhitePlayer))
      println(players(BlackPlayer).name + ":" + score(BlackPlayer))
    }
  }
}
