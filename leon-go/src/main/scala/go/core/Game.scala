package go.core

import CellObject._
import leon.collection._
import PlayerTypeObject._

// TODO: Add rule engine
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

  // TODO: move error check into rules
  def move(step: Step): Game = {
    require(Rule.check(this, step).isDefined)
    step match {
      case Pass => Game(state :: states, step :: steps)
      case Place(x, y) =>
        val newState = CaptureLogic.put(state, Point(x, y), activePlayer.cell)
        Game(newState :: states, step :: steps)
    }
  } ensuring {
    _ match {
      case _ => true
    }
  }

  override def toString(): String = state.toString()
}

object Game {
  def apply(b: Board): Game = Game(List[Board](b), List[Step]())
  def apply(n: BigInt): Game = Game(new Board(n))
}
