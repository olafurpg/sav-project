package go.util
import leon.collection.List
import leon.annotation._
import go.collection.GoSet
import leon.lang.string

object Range {
  def to(from: BigInt, to: BigInt): List[BigInt] = {
    require(from <= to && from >= 0)
    if (from == to) List(from) else from :: this.to(from + 1, to)
  } ensuring { res =>
    res.size == to - from + 1 &&
    inOrder(res, from)
  }

  def inOrder(list: List[BigInt], from: BigInt): Boolean = {
    if (list.isEmpty) true
    else list.head == from && inOrder(list.tail, from + 1)
  }

  def until(from: BigInt, to: BigInt): List[BigInt] = {
    if (from >= to) List() else from :: until(from + 1, to)
  }

  def test(list: List[BigInt], f: BigInt => Boolean): List[BigInt] = {
    list.filter(f)
  } ensuring { res =>
    res.forall(f) && list.forall(e => res.contains(e) || !f(e))
  }
}
