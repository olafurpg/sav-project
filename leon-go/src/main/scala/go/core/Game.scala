package go.core

import CellObject._
import leon.collection._
import PlayerTypeObject._

case class Game(states: List[Board], steps: List[Step]) {
  //  require(s.size > 0)

  def state: Board = {
    require(states.size.toInt > 0)
    states.head
  }

  val activePlayer: PlayerType = if (states.size.toInt % 2 == 1) BlackPlayer else WhitePlayer

  val round: BigInt = states.size.toInt - 1

  val size: BigInt = state.n

  def isOver: Boolean = {
    round > 1 && steps.head == Pass && steps.tail.head == Pass
  }

  override def toString(): String = state.toString()
}

object Game {
  def apply(b: Board): Game = Game(List[Board](b), List[Step]())
  def apply(n: BigInt): Game = Game(new Board(n))
}
