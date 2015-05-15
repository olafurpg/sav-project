package go.util
import leon.collection._

object Range {

  def to(from: Int, to: Int): List[Int] = if (from > to) Nil else from :: this.to(from + 1, to)

  def until(from: Int, to: Int): List[Int] = if (from >= to) Nil else from :: until(from + 1, to)

}
