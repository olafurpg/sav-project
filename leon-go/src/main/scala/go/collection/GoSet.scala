package go.collection

import go.core.PlacedCell
import leon.collection._
import GoSet._

case class GoSet[T](elements: List[T]) {
  def isValid: Boolean = {
    if (isEmpty) true
    else !GoSet(elements.tail).contains(elements.head) && GoSet(elements.tail).isValid
  }

  def isEmpty: Boolean = elements.isEmpty

  def size: BigInt = elements.size

  def foldLeft[R](z: R)(f: (R, T) => R): R = elements.foldLeft(z) { (r, e) => f(r, e) }

  def -(e: T): GoSet[T] = GoSet(elements.filter(_ != e))

  def --(es: GoSet[T]) = filterNot(es.contains)

  def +(e: T): GoSet[T] = {
    require(isValid)
    GoSet(e::elements)
  }

  def ++(that: GoSet[T]): GoSet[T] = {
    require(isValid && that.isValid)
    GoSet(elements ++ that.elements.filter(x => !elements.contains(x)))
  }

  def map[S](f: T => S): GoSet[S] = GoSet(elements.map(f))

  def contains(e: T): Boolean = elements.contains(e)

  def exists(f: T => Boolean): Boolean = elements.exists(x => f(x))

  def filter(f: T => Boolean): GoSet[T] = GoSet(elements.filter(x => f(x)))

  def filterNot(f: T => Boolean): GoSet[T] = GoSet(elements.filter(x => !f(x)))

  def isEqualTo(that: GoSet[T]): Boolean =
    that.elements.forall(x => elements.contains(x)) &&
      elements.forall(x => that.elements.contains(x))

  def toList = elements
}

object GoSet {
  def noDuplicates[T](lst: List[T]): Boolean = {
    if (lst.isEmpty) true
    else !lst.tail.exists(_ == lst.head) && noDuplicates(lst.tail)
  }

  def empty[T]: GoSet[T] = GoSet(List[T]())
}
