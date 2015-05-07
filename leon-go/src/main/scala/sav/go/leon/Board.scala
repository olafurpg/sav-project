package sav.go.leon

import sav.go.scala.Util

case class Board(n: Int, cells: Map[(Int, Int), Cell]) {
  import Util._

  def this(n: Int) = this(n, Map.empty)

  def inRange(x: Int) = 0 < x && x <= n

  def insideBoard(p: (Int, Int)) = inRange(p._1) && inRange(p._2)

  def at(x: Int, y: Int): Cell = cells.getOrElse((x, y), EmptyCell)

  def at(p: (Int, Int)): Cell = at(p._1, p._2)

  val r = 1 to n

  def board =
    r.map(x => r.map(y => at(x, y)))

  def put(c: Cell, p: (Int, Int)): Board = {
    require(insideBoard(p) && !cells.contains(p))
    val captured = Board(n, cells + (p -> c)).capturedCells
    Board(n, cells.filterNot(x => captured.contains(x)) + (p -> c))
  }

  def hasLiberty(p: ((Int, Int), Cell)): Boolean = neighboors(p._1).exists(_._2 == EmptyCell)

  def capturedCells: Set[((Int, Int), Cell)] = {
    val e = Set.empty[((Int, Int), Cell)]
    cells.foldRight(e -> e) { case (p, (explored, captured)) =>
        if (explored(p)) explored -> captured
        else {
          val component = connectedComponent(p)
          if (component.exists(hasLiberty)) (component ++ explored, captured)
          else (component ++ explored, captured ++ explored)
        }
    }._2
  }

  def neighboors(x: Int, y: Int): List[((Int, Int), Cell)] =
    List((x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)).filter(insideBoard).map(x => (x, at(x)))

  def neighboors(p: (Int, Int)): List[((Int, Int), Cell)] =
    neighboors(p._1, p._2)

  def neighboors(p: (Int, Int), c: Cell): List[((Int, Int), Cell)] =
    neighboors(p._1, p._2).filter(x => x._2 == c)

  def sameColorNeighbors(p: ((Int, Int), Cell)): List[((Int, Int), Cell)] =
    neighboors(p._1, p._2)

  def oppositeColorNeighbors(p: (Int, Int)): List[((Int, Int), Cell)] =
    neighboors(p, at(p).otherColor)

  def emptyNeighors(p: (Int, Int)): List[((Int, Int), Cell)] =
    neighboors(p).filter(x => x._2 == EmptyCell)

  def connectedComponent(p: ((Int, Int), Cell), visited: Set[((Int, Int), Cell)] = Set.empty): Set[((Int, Int), Cell)] = {
    if (visited.contains(p)) visited
    else {
      val newVisited = visited + p
      val toVisit = sameColorNeighbors(p)
      toVisit.foldRight(newVisited) {
        case (a, b) =>
          //        println(s"p = $p, toVisit = $toVisit, a = $a, b = $b")
          connectedComponent(a, b)
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
    board.map(_.mkString("")).mkString("\n", "\n", "\n")
  }
}

object Board {

  def fromString(N: Int, str: String): Board = {
    val cells = for {
      (row, x) <- str.split("\n").filter(!_.isEmpty).zipWithIndex
      (ch, y) <- {
        row.zipWithIndex
      }
    } yield (x + 1) -> (y + 1) -> Cell.fromString(ch)
    Board(N, Map(cells.filter(_._2 != EmptyCell): _*))
  }
}

