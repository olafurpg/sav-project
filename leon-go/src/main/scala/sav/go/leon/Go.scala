package sav.go.leon

import scala.collection.parallel.mutable

sealed trait Cell
case object WhiteCell extends Cell
case object BlackCell extends Cell
case object EmptyCell extends Cell

case class Board(n: Int, cells: Map[(Int, Int), Cell]) {
  def this(n: Int) = this(n, Map.empty)

  def inRange(x: Int) = 0 <= x && x < n

  def put(c: Cell, x: Int, y: Int) = {
    require(inRange(x) && inRange(y) && !cells.contains((x, y)))
    Board(n, cells + ((x -> y) -> c))
  }

  def full: Boolean = cells.size == n * n

  def playerCells(p: PlayerType): Set[(Int, Int)] = cells.withFilter(_._2 == p.cell).map(_._1).toSet
}

sealed trait PlayerType {
  def cell: Cell = this match {
    case WhitePlayer => WhiteCell
    case BlackPlayer => BlackCell
  }
  def nextPlayer: PlayerType = this match {
    case WhitePlayer => BlackPlayer
    case BlackPlayer => WhitePlayer
  }
  def previousPlayer: PlayerType = nextPlayer
}

case object WhitePlayer extends PlayerType
case object BlackPlayer extends PlayerType


sealed trait Step
case object Pass extends Step
case class Place(x: Int, y: Int) extends Step

case class State(b: Board, activePlayer: PlayerType) {
  def next(m: Step): State = m match {
    case Pass => State(b, activePlayer.nextPlayer)
    case Place(x, y) => State(b.put(activePlayer.cell, x, y), activePlayer.nextPlayer)
  }

  def lastStep(prevState: State): Step = {
    val k = b.playerCells(activePlayer.previousPlayer) -- prevState.b.playerCells(activePlayer)
    // Leon must verify that this is never more than one
    k.headOption.map { case (x, y) =>
        Place(x, y)
    }.getOrElse(Pass)
  }

  def isOver: Boolean = b.full
}

object State {
  def apply(n: Int): State = State(new Board(n), BlackPlayer)
}

case class Game(states: List[State]) {
//  require(s.length > 0)

  def state: State = {
    require(states.length > 0)
    states.head
  }

  def lastGame: Game = {
    require(states.length > 0)
    Game(states.tail)
  }

  def lastStep: Step = {
    require(states.length >= 2)
    state.lastStep(lastGame.state)
  }

  val activePlayer: PlayerType = state.activePlayer

  def isOver: Boolean = lastStep == Pass && lastGame.lastStep == Pass

  // TODO:
  def steps: List[Step] = ???

  def move(m: Step): Game = {
    Game(states.head.next(m) :: states)
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
  override def move(g: Game): Step = Place(0, 0)
}

case object ComputerPlayer extends Player {
  override def move(g: Game): Step = Place(0, 0)
}
