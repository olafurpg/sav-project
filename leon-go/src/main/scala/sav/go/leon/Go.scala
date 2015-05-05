package sav.go.leon

import java.text.ParseException

import scala.collection.parallel.mutable
import scala.io.StdIn

sealed trait Cell

case object WhiteCell extends Cell {
  override def toString(): String = "O"
}
case object BlackCell extends Cell {
  override def toString(): String = "X"
}
case object EmptyCell extends Cell {
  override def toString(): String = "_"
}

case class Board(n: Int, cells: Map[(Int, Int), Cell]) {
  def this(n: Int) = this(n, Map.empty)

  def inRange(x: Int) = 0 < x && x <= n

  def at(x: Int, y: Int): Cell = cells.getOrElse((x, y), EmptyCell)

  def board = {
    val r = 1 to n
    r.map(x => r.map(y => at(x, y)))
  }

  def put(c: Cell, x: Int, y: Int) = {
    require(inRange(x) && inRange(y) && !cells.contains((x, y)))
    Board(n, cells + ((x -> y) -> c))
  }

  def full: Boolean = cells.size == n * n

  def playerCells(p: PlayerType): Set[(Int, Int)] = cells.withFilter(_._2 == p.cell).map(_._1).toSet

  def next(m: Step, c: Cell): Board = m match {
    case Pass => Board(n, cells)
    case Place(x, y) => put(c, x, y)
  }
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

case class Game(states: List[Board]) {
  //  require(s.length > 0)

  def state: Board = {
    require(states.length > 0)
    states.head
  }

  def lastStep(prevState: Board): Step = {
    val k = state.playerCells(activePlayer.previousPlayer) -- prevState.playerCells(activePlayer.previousPlayer)
    // Leon must verify that this is never more than one
    k.headOption.map {
      case (x, y) =>
        Place(x, y)
    }.getOrElse(Pass)
  }

  def lastGame: Game = {
    require(states.length > 0)
    Game(states.tail)
  }

  def lastStep: Step = {
    require(states.length >= 2)
    lastStep(lastGame.state)
  }

  val activePlayer: PlayerType = if (states.length % 2 == 1) BlackPlayer else WhitePlayer

  val round: Int = states.length - 1

  val size: Int = state.n

  def isOver: Boolean = {
    if (round > 1)
      println(lastStep)
    round > 1 && (lastStep == Pass && lastGame.lastStep == Pass)
  }

  def steps: List[Step] = ???

  def move(m: Step): Game = {
    Game(state.next(m, activePlayer.cell) :: states)
  }

  override def toString(): String = state.board.map(_.mkString("")).mkString("\n")
}

object Game {
  def apply(b: Board): Game = Game(List(b))
  def apply(n: Int): Game = Game(new Board(n))
}

// ******************
// IO can happen here
// ******************

object Driver {

  def takeTurns[T](a: T, b: T)(x: T): T = x match {
    case `a` => b
    case _ => a
  }

  def main(args: Array[String]): Unit = {
    val n = 5
    val nextPlayer = takeTurns[Player](HumanPlayer, HumanPlayer) _
    val game = Stream.iterate((HumanPlayer: Player, Game(n)))({
      case (player, g) =>
        (nextPlayer(player), g.move(player.move(g)))
    }) takeWhile { case (_, g) => !g.isOver }
    game.toList
  }
}

trait Util {

  def lineDivider(n: Int): String = "*" * n

  def message(msg: String*): String = {
    val msgs = msg.toList
    val line = lineDivider(msgs.maxBy(_.length).length + 2)
    ((line :: msgs.map("* " + _)) :+ line).mkString("\n")
  }

}

trait Player {
  def move(g: Game): Step
}

case object HumanPlayer extends Player with Util {
  val coordinate = """\s*(\d+) (\d+)\s*""".r

  def readStep(g: Game): Step = {
    StdIn.readLine("x y to place, p to pass, q to quit: ") match {
      case "p" => Pass
      case "q" => {
        println(s"Goodbye!")
        System.exit(0)
        ???
      }
      case coordinate(xStr, yStr) => {
        val (x, y) = (xStr.toInt, yStr.toInt)
        if (!g.state.inRange(x) || !g.state.inRange(y)) {
          println(s"Point ($x, $y) is out of range, try again")
          readStep(g)
        }
        else if (g.state.cells.contains((x, y))) {
          println(s"Point ($x, $y) is already taken, try again")
          readStep(g)
        }
        else {
          Place(x, y)
        }
      }
      case s => {
        println(s"Input '$s' is illegal, try again")
        readStep(g)
      }
    }
  }
  override def move(g: Game): Step = {
    println(message("Round " + g.round, g.activePlayer.toString, s"Size: ${g.size} x ${g.size}"))
    println(g)
    readStep(g)
  }
}

case object ComputerPlayer extends Player {
  override def move(g: Game): Step = Place(0, 0)
}
