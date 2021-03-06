package go.collection

import go.core.definitions._
import leon.collection._
import GoMap._
import leon.annotation._

case class GoMap[K, V](pairs: List[(K, V)]) {
  @library
  def isDefinedAt(k: K): Boolean = {
    require(isValid)
    pairs.exists(_._1 == k)
  } ensuring { res =>
    (res && pairs.filter(_._1 == k).size == 1) ||
    (!res && pairs.filter(_._1 == k).size == 0)
  }

  def isValid: Boolean = {
    GoMap.noDuplicates(pairs)
  }

  def contains(k: K): Boolean = {
    require(isValid)
    isDefinedAt(k)
  } ensuring(_ == isDefinedAt(k))

  def isEmpty: Boolean = pairs.isEmpty

  def keys: List[K] = pairs.map(_._1)

  def +(pair: (K, V)): GoMap[K, V] = {
    require(isValid && !contains(pair._1))
    this + (pair._1, pair._2)
  }

  // Insertion assumes to such key exist
  @library
  def +(k: K, v: V): GoMap[K, V] = {
    require(isValid && !contains(k))
    GoMap((k, v)::pairs)
  } ensuring(res => res.isEqual(GoMap((k, v)::pairs)) && res.isValid)

  // Delete assumes such key exists
  @library
  def -(k: K): GoMap[K, V] = {
    require(isValid && contains(k))
    GoMap(pairs.filter(_._1 != k))
  } ensuring(res => isEqual(GoMap((k, get(k))::pairs)) && res.isValid)

  def --(ks: List[K]) = GoMap(pairs.filter(x => !ks.contains(x._1)))

  def filterNot(f: ((K, V)) => Boolean): GoMap[K, V] = GoMap(pairs.filter(x => !f(x)))

  def exists(f: ((K, V)) => Boolean): Boolean = pairs.exists(f)

  def forall(f: ((K, V)) => Boolean): Boolean = pairs.forall(f)

  @library
  def filter(f: ((K, V)) => Boolean): GoMap[K, V] = {
    GoMap(pairs.filter(f))
  } ensuring(res => pairs.forall(x => !f(x) || res.exists(_ == x)))

  def foldRight[R](z: R)(f: ((K, V), R) => R): R = pairs.foldRight(z)(f)

  def foldLeft[R](z: R)(f: (R, (K, V)) => R): R = pairs.foldLeft(z)(f)

  @library
  def get(k: K) = {
    require(isValid && contains(k))
    pairs.filter(_._1 == k).head._2
  } ensuring(res => pairs.contains(k -> res))

  @library
  def getOrElse(k: K, els: V): V = {
    require(isValid)
    pairs.find(_._1 == k).map(_._2).getOrElse(els)
  } ensuring { res =>
    (!contains(k) && res == els) ||
    (contains(k) && res == get(k))
  }

  def map[R](f: ((K, V)) => R): List[R] = pairs.map(f)

  def size: BigInt = pairs.size

  def isEqual(that: GoMap[K, V]): Boolean = {
    this.size == that.size &&
    forall(p => that.exists(q => q._1 == p._1 && q._2 == p._2)) &&
    that.forall(p => exists(q => q._1 == p._1 && q._2 == p._2))
  }
}

object GoMap {
  def board(list: List[PlacedCell]): GoMap[Point, Cell] = {
    GoMap(list.map(x => x.p -> x.c))
  }

  // use list operation to express predicate, as list is verified
  def noDuplicates[K, V](lst: List[(K, V)]): Boolean = {
    if (lst.isEmpty) true
    else !lst.tail.exists(lst.head._1 == _._1) && noDuplicates(lst.tail)
  }

  def empty[K, V]: GoMap[K, V] = GoMap(List[(K, V)]())
}
