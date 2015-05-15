package go.util

import go.collection.{GoNil, GoList}
import leon.collection._

object Range {

  def to(from: Int, to: Int): GoList[Int] = if (from > to) GoNil() else from :: this.to(from + 1, to)

  def until(from: Int, to: Int): GoList[Int] = if (from >= to) GoNil() else from :: until(from + 1, to)

}
