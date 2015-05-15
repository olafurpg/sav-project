package go.collection

import go.core.{Point, PlacedCell, CellObject}
import leon.collection._
import CellObject._

case class GoMap(cells: GoList[PlacedCell]) {
  require(isSorted(cells))
  def isDefinedAt(p: Point): Boolean = {
    cells.exists(_.p == p)
  }

  def contains(p: Point): Boolean = isDefinedAt(p)

  def insSort(lst: GoList[PlacedCell], v: PlacedCell): GoList[PlacedCell] = {
    require(isSorted(lst))
    lst match {
      case l if l.isEmpty => GoCons(v, GoNil())
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
    isSorted(res)
  })

  def isSorted(lst: GoList[PlacedCell]): Boolean = lst match {
    case l if l.size <= 1 => true
    case _ =>
      if (lst.tail.head < lst.head) false
      else isSorted(lst.tail)
  }

  def +(e: PlacedCell): GoMap = {
    require(isSorted(cells))
    GoMap(insSort(cells, e))
  }

  def filterNot(f: PlacedCell => Boolean): GoMap = GoMap(cells.filter(x => !f(x)))

  def filter(f: PlacedCell => Boolean): GoMap = GoMap(cells.filter(f))

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
  def empty: GoMap = GoMap(GoNil[PlacedCell]())
}
