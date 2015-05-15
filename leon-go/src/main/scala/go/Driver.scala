package go

import go.core._
import go.player._

object Driver {
  def takeTurns[T](a: T, b: T)(x: T): T = x match {
    case `a` => b
    case _ => a
  }

  def main(args: Array[String]): Unit = {
    val n = 5
    val nextPlayer = takeTurns[Player](HumanPlayer, ComputerPlayer) _
    val game = Stream.iterate((HumanPlayer: Player, Game(n)))({
      // Round

      case (player, g) =>
        val step = player.move(g)
        g.move(step) match {
          case Right(err) =>
            println(err)
            (player, g)
          case Left(game) =>
            println(s"$player performed $step")
            (nextPlayer(player), game)
        }
    }) takeWhile { case (_, g) => !g.isOver }
    game.toList
  }
}
