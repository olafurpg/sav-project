package sav.go.leon

object InclusiveRange {

  def toList(from: Int, to: Int): List[Int] = if (from > to) Nil else from :: toList(from + 1, to)

}
