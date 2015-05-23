package go.core

import leon.lang._
import go.util.Logic
import leon.collection._
import leon.annotation._
import go.collection._
import go.util.conversions._
import go.util.Range._
import go.util.Logic._
import go.core.definitions._

case class Board(n: BigInt, cells: GoMap[Point, Cell]) {
  def isValid: Boolean = n > 1 && n <= 5 && cells.keys.forall(insideBoard) && cells.isValid

  def isValidList(lst: List[PlacedCell]): Boolean = {
    require(isValid)
    lst.forall(isOnBoard) // && GoSet.noDuplicates(lst)
  }

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
  } ensuring (_.isValid)

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
        areNeighbors(pc.p, Point(x, y))
      }
  }

  def areNeighbors(p1: Point, p2: Point): Boolean = {
    ((p1.x - p2.x == 1 || p1.x - p2.x == -1) && (p1.y - p1.y == 0)) ||
      ((p1.y - p2.y == 1 || p1.y - p2.y == -1) && (p1.x - p2.x == 0))
  }

  def areColorNeighbors(pc1: PlacedCell, pc2: PlacedCell): Boolean = {
    pc1.c == pc2.c && areNeighbors(pc1.p, pc2.p)
  }

  def neighbors(p: Point): List[PlacedCell] = {
    require(isValid)
    neighbors(p.x, p.y)
  }

  def neighbors(p: Point, c: Cell): List[PlacedCell] = {
    require(isValid)
    neighbors(p.x, p.y).filter(_.c == c)
  } ensuring { res =>
    res.forall(x => x.c == c) && res.forall(isConnected(_, PlacedCell(p, c), List[PlacedCell]()))
  }

  def sameColorNeighbors(p: PlacedCell): List[PlacedCell] = {
    require(isValid)
    neighbors(p.p, p.c)
  }

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

  def noCycles(path: List[PlacedCell]): Boolean = {
    if (path.isEmpty) true
    else !path.tail.contains(path.head) && noCycles(path.tail)
  }

  def isPath(lst: List[PlacedCell]): Boolean = {
    require(isValid && isValidList(lst) && noCycles(lst))
    if (lst.size <= 1) true
    else {
      areColorNeighbors(lst.head, lst.tail.head) && isPath(lst.tail)
    }
  }

  def pathExtend(lst: List[PlacedCell], p: PlacedCell): List[PlacedCell] = {
    require(
      isValid &&
      isValidList(lst) &&
      noCycles(lst) &&
      isPath(lst) &&
      (lst.isEmpty ||
        (areColorNeighbors(p, lst.head) && !lst.contains(p))) &&
      isOnBoard(p)
    )
    p::lst
  } ensuring { res =>
    isPath(res) && noCycles(res) && isValidList(res)
  }

  def emptyPath = List[PlacedCell]()

  @induct
  def isConnected(p1: PlacedCell, p2: PlacedCell, visited: List[PlacedCell] = List[PlacedCell]()): Boolean = {
    require(isValid &&
      isOnBoard(p1) &&
      isOnBoard(p2) &&
      isValidList(visited) &&
      noCycles(visited) &&
      isPath(visited) &&
      (visited.isEmpty ||
        (areColorNeighbors(p1, visited.head) && !visited.contains(p1))) &&
     visited.forall(x => isConnected(x, p1))
    )
    if (p1 == p2) true
    else if (visited.contains(p1)) false
    else sameColorNeighbors(p1).exists(p => isConnected(p, p2, pathExtend(visited, p1)))
  } ensuring { res =>
    res == ((p1 == p2) ||
      (!visited.contains(p1) && sameColorNeighbors(p1).exists(x => isConnected(x, p2))))
  }

  @library
  def transitive(p1: PlacedCell, p2: PlacedCell, p3: PlacedCell): Boolean = {
    require(isValid && isOnBoard(p1) && isOnBoard(p2) && isOnBoard(p3))
    !(isConnected(p1, p2, List[PlacedCell]()) &&
      isConnected(p2, p3, List[PlacedCell]())) ||
    isConnected(p1, p3, List[PlacedCell]())
  }.holds

  def addToComponent(component: List[PlacedCell], e: PlacedCell): List[PlacedCell] = {
    require(isValid &&
      isOnBoard(e) &&
      isComponent(component)
    )
    e :: component
  } ensuring { res =>
    isComponent(res)
  }

  def isComponent(lst: List[PlacedCell]): Boolean = {
    require(isValid && isValidList(lst))
    if (lst.isEmpty) true
    else {
      //      lst.tail.forall(_.c == lst.head.c) &&
      lst.tail.forall(x => isConnected(lst.head, x))
    }
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

object Board {
  def empty(n: BigInt): Board = Board(n, GoMap.empty)
}
