package sav.go.leon

case class GoMap(cells: List[PlacedCell]) {
  def isDefinedAt(p: Point): Boolean = cells.exists(_.p == p)

  def contains(p: Point): Boolean = isDefinedAt(p)

  def +(e: (Point, Cell)): GoMap = GoMap(e :: cells.filter(_.p == e._1))

  def filterNot(f: PlacedCell => Boolean): GoMap = GoMap(cells.filterNot(f))

  def filter(f: PlacedCell => Boolean): GoMap = GoMap(cells.filter(f))

  def foldRight[R](z: R)(f: (PlacedCell,R) => R): R = cells.foldRight(z)(f)

  def getOrElse(p: Point, els: Cell): Cell = cells.find(_.p == p).map(_.c).getOrElse(els)


  def size: Int = cells.size

}

object GoMap {
  def empty: GoMap = GoMap(Nil)
}
