package sav.go.leon

import scala.collection.parallel.mutable

sealed trait Cell
case object WhiteCell extends Cell
case object BlackCell extends Cell
case object EmptyCell extends Cell

case class Board(b: Seq[Seq[Cell]]) {
  def this(n: Int) = this(List.fill(n)(List.fill(n)(EmptyCell)))
}

sealed trait PlayerType {
  def cell: Cell = this match {
    case WhitePlayer => WhiteCell
    case BlackPlayer => BlackCell
  }
}

case object WhitePlayer extends PlayerType
case object BlackPlayer extends PlayerType


trait Step
case object Pass extends Step
case object Surrender extends Step
case class Move(x: Int, y: Int) extends Step

case class State(b: Board, activePlayer: PlayerType) {
  def next(m: Step): State = activePlayer match {
    case WhitePlayer => State(b, BlackPlayer)
    case BlackPlayer => State(b, WhitePlayer)
  }
  def isOver: Boolean = false
}

object State {
  def apply(n: Int): State = State(new Board(n), BlackPlayer)
}

case class Game(s: List[State]) {
//  require(s.length > 0)

  val activePlayer: PlayerType = s.head.activePlayer

  def isOver: Boolean = s.head.isOver

  def move(m: Step): Game = {
    Game(s.head.next(m) :: s)
  }
}

object Game {
  def apply(s: State): Game = Game(List(s))
  def apply(n: Int): Game = Game(State(n))
}


// ******************
// IO can happen here
// ******************

object Driver {
  def main(args: Array[String]): Unit = {
    val n = 5
    var g = Game(n)
    val players = List(HumanPlayer, ComputerPlayer)
    var i = 1
    while (!g.isOver) {
      g = g.move(players(i).move(g))
      i = 1 - i
    }
  }
}

trait Player {
  def move(g: Game): Step
}

case object HumanPlayer extends Player {
  override def move(g: Game): Step = Move(0, 0)
}

case object ComputerPlayer extends Player {
  override def move(g: Game): Step = Move(0, 0)
}
