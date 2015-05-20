package go.util
import leon.collection.List
import leon.annotation._
import go.collection.GoSet

object Range {
  @library
  def to(from: BigInt, to: BigInt): List[BigInt] = {
    require(from <= to && from >= 0)
    if (from == to) List(from) else from :: this.to(from + 1, to)
  } ensuring { res =>
    res.size == to - from + 1 &&
    contains(res, from, to) &&
    GoSet.noDuplicates(res)
  }

  def contains(list: List[BigInt], from: BigInt, to: BigInt): Boolean = {
    if (from > to) true else list.contains(from) && contains(list, from + 1, to)
  }


  def until(from: BigInt, to: BigInt): List[BigInt] = {
    if (from >= to) List() else from :: until(from + 1, to)
  }

}
