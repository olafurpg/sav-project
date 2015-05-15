package go.core

import go.collection._
import go.util.conversions._

case class Point(x: Int, y: Int) {
  override def toString(): String = s"($x, $y)"
  def tpl: (Int, Int) = x -> y
}

case class PlacedCell(p: Point, c: Cell) {
  override def toString(): String = s"$c$p"
}

// TODO: move logic out, board should be ignorant of game logic
//       only care place stone, remove stone
case class Board(n: Int, cells: GoMap) {

  def this(n: Int) = this(n, GoMap.empty)

  def inRange(x: Int) = 0 < x && x <= n

  def insideBoard(p: Point) = inRange(p.x) && inRange(p.y)

  def isOccupied(p: Point) = cells.isDefinedAt(p)

  def at(x: Int, y: Int): Cell = cells.getOrElse(Point(x, y), EmptyCell)

  def at(p: Point): Cell = at(p.x, p.y)

  val r = go.util.Range.to(1, n)

  def board = r.map(x => r.map(y => at(x, y)))

  // TODO: need to remember how many stones are captured
  def put(c: Cell, p: Point): Board = {
    require(insideBoard(p) && !cells.contains(p))
    val captured1 = Board(n, cells + (p -> c)).capturedCells.filterNot(_.c == c)
    val b1 = Board(n, (cells + (p -> c)).filterNot(x => captured1.contains(x)))
    val captured2 = b1.capturedCells.filter(_.c == c)
//    println(s"captured = $captured1")
    Board(n, b1.cells.filterNot(x => captured2.contains(x)))
  }

  def freeCells: GoSet[Point] = GoSet((for {
    x <- r
    y <- r
  } yield Point(x, y)).filterNot(cells.isDefinedAt))

  def hasLiberty(p: PlacedCell): Boolean = neighboors(p.p).exists(_.c == EmptyCell)

  def capturedCells: GoSet[PlacedCell] = {
//    println(s"this = $this")
    val e = GoSet.empty[PlacedCell]
    cells.foldRight(e -> e) {
      case (p, (explored, captured)) =>
        if (explored.contains(p)) explored -> captured
        else {
          val component = connectedComponent(p)
//          println(s"p = $p, component = $component, explored = $explored, captured = $captured")
          if (component.exists(hasLiberty)) (explored ++ component, captured)
          else (explored ++ component, captured ++ component)
        }
    }._2
  }

  def neighboors(x: Int, y: Int): List[PlacedCell] =
    List((x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)).map(tpl2Point).filter(insideBoard).map(x => PlacedCell(x, at(x)))

  def neighboors(p: Point): List[PlacedCell] =
    neighboors(p.x, p.y)

  def neighboors(p: Point, c: Cell): List[PlacedCell] =
    neighboors(p.x, p.y).filter(_.c == c)

  def sameColorNeighbors(p: PlacedCell): List[PlacedCell] =
    neighboors(p.p, p.c)

  def oppositeColorNeighbors(p: Point): List[PlacedCell] =
    neighboors(p, at(p).otherColor)

  def emptyNeighors(p: Point): List[PlacedCell] =
    neighboors(p).filter(_.c == EmptyCell)

  def connectedComponent(p: PlacedCell, visited: GoSet[PlacedCell] = GoSet.empty): GoSet[PlacedCell] = {
    if (visited.contains(p)) visited
    else {

      val newVisited = visited + p
      val toVisit = sameColorNeighbors(p)
      toVisit.foldLeft(newVisited) {
        case (b, a) =>
          connectedComponent(a, b)
      }
    }
  }

  def full: Boolean = cells.size == n * n

  def playerCells(p: PlayerType): GoSet[Point] = GoSet(cells.cells.filter(_.c == p.cell).map(_.p))

  def next(m: Step, c: Cell): Board = m match {
    case Pass => Board(n, cells)
    case Place(x, y) => put(c, Point(x, y))
  }

  override def toString() = {
    board.map(_.mkString("")).mkString("\n", "\n", "\n")
  }
}

object Board {

  def fromString(N: Int, str: String): Board = {
    val cells = for {
      (row, x) <- str.stripMargin.split("\n").filter(!_.isEmpty).zipWithIndex
      (ch, y) <- {
        row.zipWithIndex
      }
    } yield PlacedCell(Point(x + 1, y + 1), Cell.fromString(ch))
    Board(N, GoMap(cells.filter(_.c != EmptyCell).toList))
  }
}
