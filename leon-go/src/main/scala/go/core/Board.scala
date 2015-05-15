package go.core

import go.collection._
import go.util.conversions._
import leon.lang.string
import CellObject._

import PlayerTypeObject._

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
    require(insideBoard(p) && !isOccupied(p))
    Board(n, cells + PlacedCell(p, c))
  }

  def freeCells: GoSet[PlacedCell] = GoSet((for {
    x <- r
    y <- r
  } yield Point(x, y)).filterNot(cells.isDefinedAt).map(x => PlacedCell(x, EmptyCell)))

  def neighboors(x: Int, y: Int): GoList[PlacedCell] =
    GoCons((x + 1, y), GoCons((x, y + 1), GoCons((x - 1, y), GoCons((x, y - 1), GoNil())))).map(tpl2Point).filter(insideBoard).map(x => PlacedCell(x, at(x)))

  def neighboors(p: Point): GoList[PlacedCell] =
    neighboors(p.x, p.y)

  def neighboors(p: Point, c: Cell): GoList[PlacedCell] =
    neighboors(p.x, p.y).filter(_.c == c)

  def sameColorNeighbors(p: PlacedCell): GoList[PlacedCell] =
    neighboors(p.p, p.c)

  def oppositeColorNeighbors(p: Point): GoList[PlacedCell] =
    neighboors(p, at(p).otherColor)

  def emptyNeighors(p: Point): GoList[PlacedCell] =
    neighboors(p).filter(_.c == EmptyCell)

  def full: Boolean = cells.size == BigInt(n).pow(2)

  def remove(p: Point): Board = Board(n, cells.filterNot(_.p == p))

  def remove(ps: GoSet[PlacedCell]): Board = Board(n, cells.filterNot(ps.contains))

  def playerCells(p: PlayerType): GoSet[PlacedCell] = GoSet(cells.cells.filter(_.c == p.cell))

}
