package go.collection

import go.core.PlacedCell
import leon.collection._
import GoSet._
import leon.annotation._

case class GoSet[T](elements: List[T]) {
  def isValid: Boolean = {
    if (isEmpty) true
    else !GoSet(elements.tail).contains(elements.head) && GoSet(elements.tail).isValid
  }

  def isEmpty: Boolean = elements.isEmpty

  def size: BigInt = elements.size

  def foldLeft[R](z: R)(f: (R, T) => R): R = elements.foldLeft(z) { (r, e) => f(r, e) }

  @library
  def -(e: T): GoSet[T] = {
    require(isValid && contains(e))
    GoSet(elements.filter(_ != e))
  } ensuring(res => isEqualTo(res + e) && res.isValid)

  def --(es: GoSet[T]) = {
    require(isValid && es.forall(contains))
    filterNot(es.contains)
  }

  @library
  def +(e: T): GoSet[T] = {
    require(isValid && !contains(e))
    GoSet(e::elements)
  } ensuring (res => res.isEqualTo(GoSet(e::elements)) && res.isValid)

  def ++(that: GoSet[T]): GoSet[T] = {
    require(isValid && that.isValid)
    GoSet(elements ++ that.elements.filter(x => !elements.contains(x)))
  }

  def map[S](f: T => S): GoSet[S] = GoSet(elements.map(f))

  def contains(e: T): Boolean = elements.contains(e)

  def exists(f: T => Boolean): Boolean = elements.exists(f)

  def forall(f: T => Boolean): Boolean = elements.forall(f)

  def filter(f: T => Boolean): GoSet[T] = GoSet(elements.filter(f))

  def filterNot(f: T => Boolean): GoSet[T] = {
    require(isValid)
    GoSet(elements.filter(!f(_)))
  } ensuring { res =>
    elements.filter(!f(_)) == res.elements
  }

  def isEqualTo(that: GoSet[T]): Boolean =
    that.elements.forall(elements.contains) && elements.forall(that.elements.contains)

  def toList = elements
}

object GoSet {
  def noDuplicates[T](lst: List[T]): Boolean = {
    if (lst.isEmpty) true
    else !lst.tail.exists(_ == lst.head) && noDuplicates(lst.tail)
  }

  def empty[T]: GoSet[T] = GoSet(List[T]())
}
