package sav.go.scala

import sav.go.leon.Game

object Driver {
  def takeTurns[T](a: T, b: T)(x: T): T = x match {
    case `a` => b
    case _ => a
  }

  def main(args: Array[String]): Unit = {
    val n = 5
    val nextPlayer = takeTurns[Player](HumanPlayer, HumanPlayer) _
    val game = Stream.iterate((HumanPlayer: Player, Game(n)))({
      // Round
      case (player, g) =>
        g.move(player.move(g)) match {
          case Right(err) =>
            println(err)
            (player, g)
          case Left(game) =>
            (nextPlayer(player), game)
        }
    }) takeWhile { case (_, g) => !g.isOver }
    game.toList
  }
}


