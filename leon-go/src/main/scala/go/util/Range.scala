package go.util
import leon.collection.List

object Range {

  def to(from: BigInt, to: BigInt): List[BigInt] = {
    require(from <= to && from >= 0)
    if (from == to) List(from) else from :: this.to(from + 1, to)
  } ensuring { res =>
    res.size == to - from + 1 && check(from, to, res)
  }

  def check(x: BigInt, to: BigInt, list: List[BigInt]): Boolean = {
    if (x > to) true else list.contains(x) && check(x + 1, to, list)
  }


  def until(from: BigInt, to: BigInt): List[BigInt] = {
    if (from >= to) List() else from :: until(from + 1, to)
  }

}
