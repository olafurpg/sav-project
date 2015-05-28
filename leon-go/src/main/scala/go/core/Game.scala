package go.core

import leon.collection._
import go.core.definitions._

case class Game(states: List[Board], steps: List[Step], activePlayer: PlayerType, size: BigInt) {
  def isValid = states.size > 0 && states.forall(_.isValid) && states.forall(_.n == size)

  def state: Board = {
    require(isValid)
    states.head
  }

  val round: BigInt = steps.size
}

object Game {
  def apply(b: Board): Game = Game(List[Board](b), List[Step](), BlackPlayer, b.n)
  def apply(n: BigInt): Game = Game(new Board(n))
}
