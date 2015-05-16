package go.collection

import go.core.{Point, PlacedCell, CellObject}
import leon.collection._
import CellObject._
import GoMap._

case class GoMap(cells: List[PlacedCell]) {
  def isDefinedAt(p: Point): Boolean = {
    cells.exists(_.p == p)
  }

  def isValid: Boolean = allValidPoints(cells) && noDuplicates(cells) && isSorted(cells)

  def contains(p: Point): Boolean = isDefinedAt(p)

  def contains(p: PlacedCell): Boolean = isDefinedAt(p.p)

  def isEmpty: Boolean = cells.isEmpty

  def ++(m: GoMap): GoMap = {
    require(this.isValid && m.isValid)
    if (m.isEmpty) this
    else (this + m.cells.head) ++ GoMap(m.cells.tail)
  } ensuring(_.isValid)

  def +(e: PlacedCell): GoMap = {
    require(isSorted(cells) && allValidPoints(cells) && e.isValid)
    GoMap(insSort(cells, e))
  } ensuring(res => res.contains(e.p))

  def filterNot(f: PlacedCell => Boolean): GoMap = GoMap(cells.filter(x => !f(x)))

  def exists(f: PlacedCell => Boolean): Boolean = cells.exists(f)

  def forall(f: PlacedCell => Boolean): Boolean = cells.forall(f)

  def filter(f: PlacedCell => Boolean): GoMap = {
    GoMap(cells.filter(f))
  } ensuring(res => cells.forall(x => !f(x) || res.contains(x.p)))

  def foldRight[R](z: R)(f: (PlacedCell,R) => R): R = cells.foldRight(z)(f)

  def foldLeft[R](z: R)(f: (R, PlacedCell) => R): R = cells.foldLeft(z)(f)

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

  def noDuplicates(lst: List[PlacedCell]): Boolean = {
    require(allValidPoints(lst))
    if (lst.isEmpty) true
    else !lst.tail.contains(lst.head) && noDuplicates(lst.tail)
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


  def empty: GoMap = GoMap(List[PlacedCell]())
}
