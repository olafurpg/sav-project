package go.collection

import leon.collection._
import GoSet._
import leon.annotation._

case class GoSet[T](elements: List[T]) {
  def isValid: Boolean = {
    GoSet.noDuplicates(elements)
  }

  def isEmpty: Boolean = elements.isEmpty

  def size: BigInt = elements.size

  def foldLeft[R](z: R)(f: (R, T) => R): R = elements.foldLeft(z) { (r, e) => f(r, e) }

  @library
  def -(e: T): GoSet[T] = {
    require(isValid)
    GoSet(elements.filter(_ != e))
  } ensuring { res =>
    (
      (!elements.contains(e) && isEqualTo(res)) ||
      (elements.contains(e) && isEqualTo(GoSet(e::res.elements)))
    ) &&
    res.isValid
  }

  def --(es: GoSet[T]) = {
    require(isValid)
    filterNot(es.contains)
  }

  @library
  def +(e: T): GoSet[T] = {
    require(isValid)
    if (elements.contains(e)) this
    else GoSet(e::elements)
  } ensuring { res =>
    (
      (elements.contains(e) && isEqualTo(res)) ||
      (!elements.contains(e) && res.isEqualTo(GoSet(e::elements)))
    ) &&
    res.isValid
  }

  @library
  def ++(that: GoSet[T]): GoSet[T] = {
    require(isValid && that.isValid)
    GoSet(elements ++ that.elements.filter(x => !elements.contains(x)))
  } ensuring(_.isValid)

  def map[S](f: T => S): GoSet[S] = GoSet(elements.map(f))

  def contains(e: T): Boolean = elements.contains(e)

  def exists(f: T => Boolean): Boolean = elements.exists(f)

  def forall(f: T => Boolean): Boolean = elements.forall(f)

  def filter(f: T => Boolean): GoSet[T] = GoSet(elements.filter(f))

  @library
  def filterNot(f: T => Boolean): GoSet[T] = {
    require(isValid)
    GoSet(elements.filter(!f(_)))
  } ensuring { res =>
    forall(p => res.contains(p) || f(p)) &&
    res.forall(!f(_)) &&
    res.isValid
  }

  @library
  def product[R](that: GoSet[R]): GoSet[(T, R)] = {
    require(isValid && that.isValid)

    GoSet(for {
      x <- elements
      y <- that.elements
    } yield (x, y))
  } ensuring(_.isValid)

  def isEqualTo(that: GoSet[T]): Boolean =
    that.elements.forall(elements.contains) && elements.forall(that.elements.contains)

  def toList = elements
}

object GoSet {
  // use list operation to express predicate, as list is verified
  def noDuplicates[T](lst: List[T]): Boolean = {
    lst.forall(x => lst.filter(_ == x).size == 1)
  }

  def empty[T]: GoSet[T] = GoSet(List[T]())
}
