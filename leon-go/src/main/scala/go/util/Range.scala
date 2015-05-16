package go.util
import leon.collection.List

object Range {

  def to(from: BigInt, to: BigInt): List[BigInt] = if (from > to) List() else from :: this.to(from + 1, to)

  def until(from: BigInt, to: BigInt): List[BigInt] = if (from >= to) List() else from :: until(from + 1, to)

}
