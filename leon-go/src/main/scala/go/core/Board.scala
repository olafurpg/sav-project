package go.core

import leon.collection._
import leon.annotation._
import go.collection._
import go.util.conversions._
import go.util.Range._
import go.core.definitions._

case class Board(n: BigInt, cells: GoMap[Point, Cell]) {
  def isValid: Boolean = n > 1 && n <= 10 && cells.keys.forall(insideBoard) && cells.isValid

  def this(n: BigInt) = this(n, GoMap.empty)

  def inRange(x: BigInt) = 0 < x && x <= n

  def insideBoard(p: Point): Boolean = inRange(p.x) && inRange(p.y)

  def insideBoard(pc: PlacedCell): Boolean = insideBoard(pc.p)

  def isOccupied(p: Point) = {
    require(isValid)
    cells.isDefinedAt(p)
  }

  def isOnBoard(p: PlacedCell) = {
    require(isValid)
    insideBoard(p.p) && at(p.p) == p.c
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

  def board = {
    require(isValid)
    to(1, n).map(x => to(1, n).map(y => at(x, y)))
  }

  @library
  def put(c: Cell, p: Point): Board = {
    require(isValid && insideBoard(p) && !isOccupied(p))
    Board(n, cells + (p, c))
  } ensuring { res =>
    n == res.n && cells + (p, c) == res.cells && res.isValid
  }

  @library
  def allPoints: GoSet[Point] = {
    val range = GoSet(to(1, n))
    range.product(range).map(p => Point(p._1, p._2))
  } ensuring(_.isValid)

  @library
  def freeCells: GoSet[Point] = {
    require(isValid)
    allPoints.filterNot(isOccupied)
  } ensuring { res =>
    allPoints.forall(p => res.contains(p) || isOccupied(p)) &&
    res.forall(!isOccupied(_))
  }

  def neighbors(x: BigInt, y: BigInt): List[PlacedCell] = {
    require(isValid)
    List((x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)).map(bigIntTuple2Point).filter(insideBoard).map(x => PlacedCell(x, at(x)))
  } ensuring { res =>
    res.forall(insideBoard) &&
    res.forall { pc =>
      ((pc.p.x - x == 1 || pc.p.x - x == -1) && (pc.p.y - y == 0) ) ||
      ((pc.p.y - y == 1 || pc.p.y - y == -1) && (pc.p.x - x == 0) )
    }
  }

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

  def full: Boolean = {
    require(isValid)
    cells.size == n * n
  }

  @library
  def remove(p: Point): Board = {
    require(isValid && insideBoard(p) && isOccupied(p))
    Board(n, cells - p)
  } ensuring { res =>
    n == res.n && (res.cells + (p, cells.get(p))).isEqual(cells) && res.isValid
  }

  @library
  def remove(ps: GoSet[PlacedCell]): Board = {
    require(isValid && ps.isValid && ps.forall(isOnBoard))
    Board(n, cells -- ps.map(_.p))
  } ensuring { res =>
    n == res.n && ps.foldLeft(res.cells) { (acc, p) => acc + (p.p -> p.c) }.isEqual(cells)
  }

  def isEqual(that: Board) = this.n == that.n && this.cells.isEqual(that.cells)
}
