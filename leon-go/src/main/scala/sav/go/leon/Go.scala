package sav.go.leon

import java.text.ParseException

import scala.collection.parallel.mutable
import scala.io.StdIn

object Cell {
  def fromString(ch: Char): Cell = ch match {
          case 'X' | 'x' => BlackCell
          case 'O' | 'o' => WhiteCell
          case _ => EmptyCell
  }
}
sealed abstract class Cell {
  def otherColor: Cell = this match {
    case WhiteCell => BlackCell
    case BlackCell => WhiteCell
    case EmptyCell => EmptyCell
  }
}

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

  def insideBoard(p: (Int, Int)) = inRange(p._1) && inRange(p._2)

  def at(x: Int, y: Int): Cell = cells.getOrElse((x, y), EmptyCell)

  def at(p: (Int, Int)): Cell = at(p._1, p._2)

  val r = 1 to n

  def board = {
    r.map(x => r.map(y => at(x, y)))
  }

  // TODO: disallow suicide
  def put(c: Cell, p: (Int, Int)) = {
    require(insideBoard(p) && !cells.contains(p))
    Board(n, cells.filter(x => at(x._1) == c.otherColor && isCaptured(p)(x)) + (p -> c))
  }

  // TODO: take into account color
  def isCaptured(newP: (Int, Int))(p: ((Int, Int), Cell)): Boolean = {
    dfs(p._1).exists(p => !emptyNeighors(p).filter(_ != newP).isEmpty)
  }

  def hasLiberty(p: (Int, Int)): Boolean = neighboors(p).map(at).contains(EmptyCell)

  def neighboors(p: (Int, Int)): List[(Int, Int)] =
    neighboors(p._1, p._2)

  def sameColorNeighbors(p: (Int, Int)): List[(Int, Int)] = {
    val c = at(p)
    neighboors(p).filter(x => at(x) == c)
  }

  def emptyNeighors(p: (Int, Int)): List[(Int, Int)] = {
    neighboors(p).filter(x => at(x) == EmptyCell)
  }

  def neighboors(x: Int, y: Int): List[(Int, Int)] =
    List((x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)).filter(insideBoard)

  def dfs(p: (Int, Int)): Set[(Int, Int)] = dfs(p, Set.empty)

  def dfs(p: (Int, Int), visited: Set[(Int, Int)]): Set[(Int, Int)] = {
    if (visited.contains(p)) visited
    else {
      val newVisited = visited + p
      val toVisit = sameColorNeighbors(p)
      toVisit.foldRight(newVisited) { case (a, b) =>
//        println(s"p = $p, toVisit = $toVisit, a = $a, b = $b")
        dfs(a, b)
      }
    }
  }

  def full: Boolean = cells.size == n * n

  def playerCells(p: PlayerType): Set[(Int, Int)] = cells.withFilter(_._2 == p.cell).map(_._1).toSet

  def next(m: Step, c: Cell): Board = m match {
    case Pass => Board(n, cells)
    case Place(x, y) => put(c, x -> y)
  }

  override def toString() = {
    board.map(_.mkString("")).mkString("\n")
  }
}
object Board {
  def fromString(N: Int, str: String): Board = {
    val cells = for {
      (row, x) <- str.split("\n").filter(!_.isEmpty).zipWithIndex
      (ch, y) <- {
        println(s"$row")
        row.zipWithIndex
      }
    } yield (x + 1) -> (y + 1) -> Cell.fromString(ch)
    println(cells.toList)
    Board(N, Map(cells.filter(_._2 != EmptyCell):_*))
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

case class Game(states: List[Board], steps: List[Step]) {
  //  require(s.length > 0)

  def state: Board = {
    require(states.length > 0)
    states.head
  }

  val activePlayer: PlayerType = if (states.length % 2 == 1) BlackPlayer else WhitePlayer

  val round: Int = states.length - 1

  val size: Int = state.n

  def isOver: Boolean = {
    round > 1 && steps.head == Pass && steps.tail.head == Pass
  }

  def move(m: Step): Game = {
    Game(state.next(m, activePlayer.cell) :: states, m :: steps)
  }

  override def toString(): String = state.toString()
}

object Game {
  def apply(b: Board): Game = Game(List(b), Nil)
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
