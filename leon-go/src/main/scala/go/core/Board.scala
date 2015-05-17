package go.core

import leon.collection._
import go.collection._
import go.util.conversions._
import CellObject._
import PlayerTypeObject._

case class Board(n: BigInt, cells: GoMap[Point, Cell]) {

  def isValid: Boolean = n > 0 && n <= 10 && cells.keys.forall(insideBoard) && cells.isValid

  def this(n: BigInt) = this(n, GoMap.empty)

  def inRange(x: BigInt) = 0 < x && x <= n

  def insideBoard(p: Point): Boolean = inRange(p.x) && inRange(p.y)

  def insideBoard(pc: PlacedCell): Boolean = insideBoard(pc.p)

  def isOccupied(p: Point) = {
    require(isValid)
    cells.isDefinedAt(p)
  }

  def at(x: BigInt, y: BigInt): Cell = {
    require(isValid)
    cells.getOrElse(Point(x, y), EmptyCell)
  } ensuring { res =>
    (cells.contains(Point(x, y)) && res == cells.get(Point(x, y))) ||
    (!cells.contains(Point(x, y)) && res == EmptyCell)
  }

  def at(p: Point): Cell = {
    require(isValid)
    at(p.x, p.y)
  }

  val r = go.util.Range.to(1, n)

  def board = {
    require(isValid)
    r.map(x => r.map(y => at(x, y)))
  }

  def put(c: Cell, p: Point): Board = {
    require(isValid && insideBoard(p) && !isOccupied(p))
    Board(n, cells + (p, c))
  } ensuring { res =>
    n == res.n && cells + (p, c) == res.cells
  }

  def freeCells: GoSet[PlacedCell] = {
    require(isValid)
    GoSet(for {
      x <- r
      y <- r
    } yield PlacedCell(Point(x, y), EmptyCell)).filterNot(x => cells.isDefinedAt(x.p))
  }

  def neighbors(x: BigInt, y: BigInt): List[PlacedCell] = {
    require(isValid)
    List((x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)).map(bigIntTuple2Point).filter(insideBoard).map(x => PlacedCell(x, at(x)))
  } ensuring (_.forall(insideBoard))

  def neighbors(p: Point): List[PlacedCell] = {
    require(isValid)
    neighbors(p.x, p.y)
  }

  def neighbors(p: Point, c: Cell): List[PlacedCell] = {
    require(isValid)
    neighbors(p.x, p.y).filter(_.c == c)
  }

  def sameColorNeighbors(p: PlacedCell): List[PlacedCell] = {
    require(isValid)
    neighbors(p.p, p.c)
  } ensuring(_.forall(x => x.c == p.c))

  def oppositeColorNeighbors(p: Point): List[PlacedCell] = {
    require(isValid)
    neighbors(p, at(p).otherColor)
  }

  def emptyNeighors(p: Point): List[PlacedCell] = {
    require(isValid)
    neighbors(p).filter(_.c == EmptyCell)
  }

  def full: Boolean = cells.size == n * n

  def remove(p: Point): Board = {
    require(isValid && insideBoard(p) && isOccupied(p))
    Board(n, cells - p)
  } ensuring { res =>
    n == res.n && (res.cells + (p, cells.get(p))) == cells
  }

  def remove(ps: GoSet[Point]): Board = Board(n, cells -- ps)

  def isEqual(that: Board) = this.n == that.n && this.cells.isEqual(that.cells)
}
