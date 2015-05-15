package go.collection

import leon.collection._
import leon.lang._


case class GoSet[PlacedCell](cells: GoList[PlacedCell]) {
  def size: BigInt = {
    cells.size
  } ensuring(_ >= 0)

  def foldLeft[R](z: R)(f: (R, PlacedCell) => R): R = cells.foldLeft(z)(f)

  def -(e: PlacedCell): GoSet[PlacedCell] = {
    GoSet[PlacedCell](cells - e)
  } ensuring(!_.contains(e))
  // Doesn't terminate
  // ensuring(!_.contains(e))

  def +(e: PlacedCell): GoSet[PlacedCell] = {
    GoSet[PlacedCell](e :: cells.filter(_ != e))
  } ensuring(res => res.contains(e))

  def ++(that: GoSet[PlacedCell]): GoSet[PlacedCell] = GoSet[PlacedCell](cells ++ that.cells.filter(x => !cells.contains(x)))

  def contains(e: PlacedCell): Boolean = {
    exists(_ == e)
  }
  // Doesn't terminate
  //  ensuring { res =>
  //    res == find(_ == e).isDefined
  //  }

  def find(f: PlacedCell => Boolean): GoOption[PlacedCell] = {
    if (cells.isEmpty) GoNone[PlacedCell]()
    else if (f(cells.head)) GoSome(cells.head)
    else GoSet(cells.tail).find(f)
  }

  def exists(f: PlacedCell => Boolean): Boolean = {
    if (cells.isEmpty) false
    else if (f(cells.head)) true
    else GoSet(cells.tail).exists(f)
  }

  def map[U](f: PlacedCell => U): GoList[U] = {
    cells.map(f)
  }

  def filter(f: PlacedCell => Boolean): GoSet[PlacedCell] = GoSet(cells.filter(f))

  def filterNot(f: PlacedCell => Boolean): GoSet[PlacedCell] = GoSet(cells.filter(x => !f(x)))

  def isEqualTo(that: GoSet[PlacedCell]): Boolean =
    that.cells.forall(cells.contains) && cells.forall(that.cells.contains)
}

object GoSet {
  def empty[T]: GoSet[T] = GoSet[T](GoNil[T]())
}
