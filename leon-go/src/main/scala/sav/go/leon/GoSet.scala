package sav.go.leon

case class GoSet[T](cells: List[T]) {
  def size: Int = cells.size


  def foldLeft[R](z: R)(f: (R, T) => R): R = cells.foldLeft(z)(f)

  def -(e: T): GoSet[T] = GoSet[T](cells.filterNot(_ == e))

  override def equals(that: Any): Boolean = that match {
    case GoSet(thoseCells: List[T]) => thoseCells.forall(cells.contains) && cells.forall(thoseCells.contains)
  }

}

