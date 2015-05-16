package go.collection

import go.core.{Point, PlacedCell, CellObject}
import leon.collection._
import CellObject._
import GoMap._

case class GoMap(cells: List[PlacedCell]) {
  def isDefinedAt(p: Point): Boolean = {
    cells.exists(_.p == p)
  }

  def contains(p: Point): Boolean = isDefinedAt(p)

  def isEmpty: Boolean = cells.isEmpty

  def insSort(lst: List[PlacedCell], v: PlacedCell): List[PlacedCell] = {
    require(isSorted(lst) && allValidPoints(lst) && v.isValid)
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
    isSorted(res) && allValidPoints(res)
  })

  def +(e: PlacedCell): GoMap = {
    require(isSorted(cells) && allValidPoints(cells) && e.isValid)
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
    lst.forall(_.isValid)
  }

  def isSorted(lst: List[PlacedCell]): Boolean = {
    require(allValidPoints(lst))
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
