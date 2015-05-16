package go.core

import leon.collection._
import go.collection._
import go.util.conversions._
import CellObject._
import go.util.LogicUtil._
import PlayerTypeObject._

case class Board(n: BigInt, cells: GoMap) {

  def isValid: Boolean = Point.insideRange(n) && cells.isValid

  def this(n: BigInt) = this(n, GoMap.empty)

  def inRange(x: BigInt) = 0 < x && x <= n

  def insideBoard(p: Point) = inRange(p.x) && inRange(p.y)

  def insideBoard(pc: PlacedCell) = inRange(pc.p.x) && inRange(pc.p.y)

  def isOccupied(p: Point) = cells.isDefinedAt(p)

  def at(x: BigInt, y: BigInt): Cell = cells.getOrElse(Point(x, y), EmptyCell)

  def at(p: Point): Cell = at(p.x, p.y)

  def at(pc: PlacedCell): Cell = at(pc.p.x, pc.p.y)

  def empty(p: Point): Boolean = at(p.x, p.y) == EmptyCell

  def empty(pc: PlacedCell): Boolean = empty(pc.p)

  val r = go.util.Range.to(1, n)

  def board = r.map(x => r.map(y => at(x, y)))

  def allPoints = for {
    x <- r
    y <- r
  } yield Point(x, y)

  def allPlacedCells = allPoints.map(x => PlacedCell(x, at(x)))

  def put(c: Cell, p: Point): Board = {
    require(isValid && insideBoard(p) && !isOccupied(p))
    Board(n, cells + PlacedCell(p, c))
  } ensuring (_.at(p) == c)

  def freeCells: GoSet = GoSet(GoMap(for {
    x <- r
    y <- r
  } yield PlacedCell(Point(x, y), EmptyCell)).filterNot(x => cells.isDefinedAt(x.p)))

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
  } ensuring(_.forall(x => x.c == at(p)))

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
    require(isValid && p.isValid)
    Board(n, cells.filterNot(_.p == p))
  } ensuring(_.at(p) == EmptyCell)

  def remove(ps: GoSet): Board = {
    require(isValid && ps.isValid)
    Board(n, cells.filterNot(ps.contains))
  } ensuring {
    res => allPlacedCells.forall { x => {
        implies(ps.contains(x), res.empty(x)) &&
          implies(!ps.contains(x), x.c == res.at(x))
      }
    }
  }

  def playerCells(p: PlayerType): GoSet = GoSet(cells.filter(_.c == p.cell))

}
