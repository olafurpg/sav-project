package go.collection

import go.core.{Point, PlacedCell, CellObject}
import leon.collection._
import CellObject._

case class GoMap(cells: List[PlacedCell]) {
  require(isSorted(cells))
  def isDefinedAt(p: Point): Boolean = {
    cells.exists(_.p == p)
  }

  def contains(p: Point): Boolean = isDefinedAt(p)

  def insSort(lst: List[PlacedCell], v: PlacedCell): List[PlacedCell] = {
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
    println(res)
    isSorted(res)
  })

  def isSorted(lst: List[PlacedCell]): Boolean = lst match {
    case l if l.size <= 1 => true
    case _ =>
      if (lst.tail.head < lst.head) false
      else isSorted(lst.tail)
  }

  def +(e: PlacedCell): GoMap = GoMap(insSort(cells, e))

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

  def isEqualTo(that: GoMap): Boolean =  {
    cells.forall(that.cells.contains) == that.cells.forall(cells.contains)
  }

}

object GoMap {
  def empty: GoMap = GoMap(List[PlacedCell]())
}
