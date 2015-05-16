package go.collection

import go.core.{Point, PlacedCell, CellObject}
import leon.collection._
import CellObject._

case class GoMap(cells: List[PlacedCell]) {
  def isDefinedAt(p: Point): Boolean = {
    cells.exists(_.p == p)
  }

  def contains(p: Point): Boolean = isDefinedAt(p)

  def isEmpty: Boolean = cells.isEmpty

  def insSort(lst: List[PlacedCell], v: PlacedCell): List[PlacedCell] = {
    require(GoMap.isSorted(lst) && GoMap.allValidPoints(lst) && v.isValid)
//    require(GoMap.isValid(lst) && v.p.insideRange)
    lst match {
      case l if l.isEmpty => List(v)
      case _ =>
        if (v < lst.head) {
          v :: lst
        } else if (v == lst.head) {
          lst
        } else {
          lst.head :: insSort(lst.tail, v)
        }
    }
  } ensuring(res => {
    GoMap.isSorted(res) && GoMap.allValidPoints(res)
  })

  def +(e: PlacedCell): GoMap = {
    require(GoMap.isSorted(cells) && GoMap.allValidPoints(cells) && e.isValid)
    GoMap(insSort(cells, e))
  } ensuring(res => res.contains(e.p))

  def filterNot(f: PlacedCell => Boolean): GoMap = GoMap(cells.filter(x => !f(x)))

  def filter(f: PlacedCell => Boolean): GoMap = {
    GoMap(cells.filter(f))
  } ensuring(res => cells.forall(x => !f(x) || res.contains(x.p)))


  def foldRight[R](z: R)(f: (PlacedCell,R) => R): R = cells.foldRight(z)(f)

  def getOrElse(p: Point, els: Cell): Cell = {
    cells.find(_.p == p).map(_.c).getOrElse(els)
  } ensuring { res =>
    if (!contains(p)) res == els
    else true
  }

  def size: BigInt = cells.size
}

object GoMap {
  def allValidPoints(lst: List[PlacedCell]): Boolean = {
    lst.forall(_.p.insideRange)
  }

  def isSorted(lst: List[PlacedCell]): Boolean = {
    require(lst.forall(_.isValid))
    lst match {
      case l if l.size <= 1 => true
      case _ =>
        if (lst.tail.head < lst.head) false
        else isSorted(lst.tail)
    }
  }

  def construct(cells: List[PlacedCell]): GoMap = {
    require(allValidPoints(cells))
    GoMap(cells)
  }

  def empty: GoMap = GoMap(List[PlacedCell]())
}
