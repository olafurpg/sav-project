package go.collection

import go.core._
import leon.collection._

case class GoMap(cells: List[PlacedCell]) {
  def isDefinedAt(p: Point): Boolean = cells.exists(_.p == p)

  def contains(p: Point): Boolean = isDefinedAt(p)

  def +(e: PlacedCell): GoMap = GoMap(e :: cells.filterNot(_.p == e.p))

  def filterNot(f: PlacedCell => Boolean): GoMap = GoMap(cells.filter(x => !f(x)))

  def filter(f: PlacedCell => Boolean): GoMap = GoMap(cells.filter(f))

  def foldRight[R](z: R)(f: (PlacedCell,R) => R): R = cells.foldRight(z)(f)

  def getOrElse(p: Point, els: Cell): Cell = cells.find(_.p == p).map(_.c).getOrElse(els)

  def size: Int = cells.size.toInt

  override def equals(that: Any): Boolean = that match {
    case GoMap(c) => cells.forall(c.contains) == c.forall(cells.contains)
    case _ => false
  }

}

object GoMap {
  def empty: GoMap = GoMap(List[PlacedCell]())
}
