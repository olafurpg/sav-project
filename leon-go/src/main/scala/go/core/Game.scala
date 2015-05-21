package go.core

import leon.collection._
import go.core.definitions._

case class Game(states: List[Board], steps: List[Step], activePlayer: PlayerType) {
  def state: Board = {
    require(states.size > 0)
    states.head
  }

  val round: BigInt = states.size - 1

  val size: BigInt = state.n
}

object Game {
  def apply(b: Board): Game = Game(List[Board](b), List[Step](), BlackPlayer)
  def apply(n: BigInt): Game = Game(new Board(n))
}
