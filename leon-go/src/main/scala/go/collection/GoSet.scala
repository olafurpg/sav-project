package go.collection

import leon.collection._

case class GoSet[T](cells: List[T]) {
  def size: Int = cells.size.toInt

  def foldLeft[R](z: R)(f: (R, T) => R): R = cells.foldLeft(z)(f)

  def -(e: T): GoSet[T] = GoSet[T](cells.filter(_ != e))

  def +(e: T): GoSet[T] = GoSet[T](e :: cells.filter(_ != e))

  def ++(that: GoSet[T]): GoSet[T] = GoSet[T](cells ++ that.cells.filter(x => !cells.contains(x)))

  def contains(e: T): Boolean = cells.contains(e)

  def exists(f: T => Boolean): Boolean = cells.exists(f)

  def filter(f: T => Boolean): GoSet[T] = GoSet(cells.filter(f))

  def filterNot(f: T => Boolean): GoSet[T] = GoSet(cells.filter(x => !f(x)))

  def isEqualTo(that: GoSet[T]): Boolean =
    that.cells.forall(cells.contains) && cells.forall(that.cells.contains)

}

object GoSet {
  def empty[T]: GoSet[T] = GoSet(Nil)
}
