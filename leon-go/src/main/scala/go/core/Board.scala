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
    lst.forall(isOnBoard)
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
  }

  def sameColorNeighbors(p: PlacedCell): List[PlacedCell] = {
    require(isValid)
    neighbors(p.p, p.c)
  } ensuring { res =>
    //    validList(res) &&
    res.size <= 4 &&
      res.forall(isOnBoard) &&
      res.forall(x => x.c == p.c) && neighbors(p.p).forall { x =>
        iff(x.c == p.c, res.contains(x))
      }
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

  def noCycles(path: List[PlacedCell]) = GoSet.noDuplicates(path)

  def isPath(lst: List[PlacedCell]): Boolean = {
    require(isValidList(lst) && noCycles(lst) && !lst.isEmpty)
    if (lst.size == 1) true
    else {
      areColorNeighbors(lst.head, lst.tail.head) && isPath(lst.tail)
    }
  }

  def emptyPath = List[PlacedCell]()

  def findPathIter(toVisit: List[PlacedCell], p2: PlacedCell, accumulatedVisits: List[PlacedCell]): (List[PlacedCell], List[PlacedCell]) = {
    require(isValid &&
      isValidList(p2 :: (toVisit ++ accumulatedVisits))
    )
    if (toVisit.isEmpty || accumulatedVisits.contains(toVisit.head)) (emptyPath, accumulatedVisits) // We visited all neighbors
    else {
      val p3 = toVisit.head
      val (curr, accumulatedVisits2) = findPath(p3, p2, p3 :: accumulatedVisits)
      if (curr.isEmpty) (p3 :: curr, accumulatedVisits2)
      else findPathIter(toVisit.tail, p2, accumulatedVisits2)
    }
  } ensuring { res =>
    val (p, v) = res
    v.size >= accumulatedVisits.size
  }

  def findPath(p1: PlacedCell, p2: PlacedCell, visited: List[PlacedCell] = List[PlacedCell]()): (List[PlacedCell], List[PlacedCell]) = {
    require(isValid &&
      isValidList(p1 :: p2 :: visited) &&
      !visited.contains(p1) &&
      !visited.contains(p2)
    )
    if (p1 == p2) (List[PlacedCell](p1), p1 :: visited)
    else if (visited.contains(p1)) (emptyPath, visited)
    else {
      val toVisit = sameColorNeighbors(p1).filterNot(visited.contains)
      // Optimization to accumulate visits after each round
      // Could be done with fold
      findPathIter(toVisit, p2, p1 :: visited)
    }
  } ensuring { res =>
    val (p, v) = res
    //    (p.isEmpty || isPath(p)) &&
    v.size >= visited.size
  }

  def isConnected(p1: PlacedCell, p2: PlacedCell, visited: List[PlacedCell] = List[PlacedCell]()): Boolean = {
    require(isValid &&
      isValidList(List(p1, p2)) &&
      isValidList(visited) &&
      visited.forall(x => isConnected(x, p1))
    )
    if (p1 == p2) true
    else if (visited.contains(p1)) false // ???
    else sameColorNeighbors(p1).exists(p => isConnected(p, p2, p1 :: visited))
  }

  def dfs(toVisit: List[PlacedCell], visited: List[PlacedCell] = List[PlacedCell]()): List[PlacedCell] = {
    require(isValid &&
      isComponent(CaptureLogic.addElements(this, toVisit, visited)) &&
      visited.forall(isOnBoard) &&
      toVisit.forall(isOnBoard)
    )
    if (toVisit.isEmpty) visited
    else if (visited.contains(toVisit.head)) dfs(toVisit.tail, visited)
    else {
      val lst = sameColorNeighbors(toVisit.head)
      dfs(CaptureLogic.addElements(this, toVisit.tail, lst), toVisit.head :: visited)
    }
  } ensuring { res =>
    (toVisit.content ++ visited.content).subsetOf(res.content) &&
      isComponent(res)
  }

  def addComponents(a: List[PlacedCell], b: List[PlacedCell]): List[PlacedCell] = {
    require(isValid && isComponent(a) && isComponent(b))
    CaptureLogic.addElements(this, a, b)
  } ensuring { res =>
    isComponent(res)
  }

  def addToComponent(component: List[PlacedCell], e: PlacedCell): List[PlacedCell] = {
    require(isValid &&
      isOnBoard(e) &&
      isComponent(component)
    //      component.exists(x => connected(x, e))
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

  def dfsFrom(p1: PlacedCell) = {
    require(isOnBoard(p1))
    dfs(List[PlacedCell](p1), List[PlacedCell]())
  } ensuring { res =>
    res.contains(p1) && res.forall(x => isConnected(p1, x))
  }

  def connectedIsTransitive(p1: PlacedCell, p2: PlacedCell, p3: PlacedCell): Boolean = {
    require(isValid && isValidList(List(p1, p2, p3)) && isConnected(p1, p2) && isConnected(p2, p3))
    isConnected(p1, p3)
  }.holds

  @induct
  def dfsTest1(toVisit: List[PlacedCell], visited: List[PlacedCell]): Boolean = {
    require(isValid && toVisit.forall(isOnBoard) && visited.forall(isOnBoard))
    val result = dfs(toVisit)
    visited.forall(result.contains)
  }.holds

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
