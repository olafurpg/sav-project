package go.collection

import go.core.PlacedCell
import leon.collection._

case class GoSet(m: GoMap) {
  def isValid: Boolean = m.isValid

  def size: BigInt = m.size

  def foldLeft[R](z: R)(f: (R, PlacedCell) => R): R = m.foldLeft(z)(f)

  def -(e: PlacedCell): GoSet = GoSet(m.filter(_ != e))

  def +(e: PlacedCell): GoSet = {
    require(isValid && e.isValid)
    GoSet(m + e)
  }

  def ++(that: GoSet): GoSet = {
    require(isValid && that.isValid)
    GoSet(m ++ that.m.filter(x => !m.contains(x.p)))
  }

  def contains(e: PlacedCell): Boolean = m.contains(e.p)

  def exists(f: PlacedCell => Boolean): Boolean = m.exists(f)

  def filter(f: PlacedCell => Boolean): GoSet = GoSet(m.filter(f))

  def filterNot(f: PlacedCell => Boolean): GoSet = GoSet(m.filter(x => !f(x)))

  def isEqualTo(that: GoSet): Boolean =
    that.m.forall(m.contains) && m.forall(that.m.contains)

}

object GoSet {
  def empty: GoSet = GoSet(GoMap.empty)
}
