package go.collection

import leon.collection._

case class GoSet[T](cells: List[T]) {
  def size: Int = cells.size


  def foldLeft[R](z: R)(f: (R, T) => R): R = cells.foldLeft(z)(f)

  def -(e: T): GoSet[T] = GoSet[T](cells.filterNot(_ == e))

  def +(e: T): GoSet[T] = GoSet[T](e :: cells.filterNot(_ == e))

  def ++(that: GoSet[T]): GoSet[T] = GoSet[T](cells ++ that.cells.filterNot(cells.contains))

  def contains(e: T): Boolean = cells.contains(e)

  def exists(f: T => Boolean): Boolean = cells.exists(f)

  def filter(f: T => Boolean): GoSet[T] = GoSet(cells.filter(f))

  def filterNot(f: T => Boolean): GoSet[T] = GoSet(cells.filterNot(f))

  override def equals(that: Any): Boolean = that match {
    case GoSet(thoseCells: List[T]) => thoseCells.forall(cells.contains) && cells.forall(thoseCells.contains)
    case _ => false
  }

}

object GoSet {
  def empty[T]: GoSet[T] = GoSet(Nil)
}
