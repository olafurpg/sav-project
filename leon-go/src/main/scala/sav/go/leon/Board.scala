package sav.go.leon

case class Point(x: Int, y: Int) {
  def tpl: (Int, Int) = x -> y
}

case class Board(n: Int, cells: Map[Point, Cell]) {

  def this(n: Int) = this(n, Map.empty)

  def inRange(x: Int) = 0 < x && x <= n

  def insideBoard(p: Point) = inRange(p.x) && inRange(p.y)

  def at(x: Int, y: Int): Cell = cells.getOrElse(Point(x, y), EmptyCell)

  def at(p: Point): Cell = at(p.x, p.y)

  val r = 1 to n

  def board =
    r.map(x => r.map(y => at(x, y)))

  def put(c: Cell, p: Point): Board = {
    require(insideBoard(p) && !cells.contains(p))
    val captured = Board(n, cells + (p -> c)).capturedCells
    Board(n, cells.filterNot(x => captured.contains(x)) + (p -> c))
  }

  def hasLiberty(p: (Point, Cell)): Boolean = neighboors(p._1).exists(_._2 == EmptyCell)

  def capturedCells: Set[(Point, Cell)] = {
    val e = Set.empty[(Point, Cell)]
    cells.foldRight(e -> e) { case (p, (explored, captured)) =>
        if (explored(p)) explored -> captured
        else {
          val component = connectedComponent(p)
          if (component.exists(hasLiberty)) (component ++ explored, captured)
          else (component ++ explored, captured ++ explored)
        }
    }._2
  }

  def neighboors(x: Int, y: Int): List[(Point, Cell)] =
    List((x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)).map(tpl2Point).filter(insideBoard).map(x => (x, at(x)))

  def neighboors(p: Point): List[(Point, Cell)] =
    neighboors(p.x, p.y)

  def neighboors(p: Point, c: Cell): List[(Point, Cell)] =
    neighboors(p.x, p.y).filter(x => x._2 == c)

  def sameColorNeighbors(p: (Point, Cell)): List[(Point, Cell)] =
    neighboors(p._1, p._2)

  def oppositeColorNeighbors(p: Point): List[(Point, Cell)] =
    neighboors(p, at(p).otherColor)

  def emptyNeighors(p: Point): List[(Point, Cell)] =
    neighboors(p).filter(x => x._2 == EmptyCell)

  def connectedComponent(p: (Point, Cell), visited: Set[(Point, Cell)] = Set.empty): Set[(Point, Cell)] = {
    if (visited.contains(p)) visited
    else {
      val newVisited = visited + p
      val toVisit = sameColorNeighbors(p)
      toVisit.foldRight(newVisited) {
        case (a, b) =>
          connectedComponent(a, b)
      }
    }
  }

  def full: Boolean = cells.size == n * n

  def playerCells(p: PlayerType): Set[Point] = cells.withFilter(_._2 == p.cell).map(_._1).toSet

  def next(m: Step, c: Cell): Board = m match {
    case Pass => Board(n, cells)
    case Place(x, y) => put(c, Point(x, y))
  }

  override def toString() = {
    board.map(_.mkString("")).mkString("\n", "\n", "\n")
  }
}

object Board {

  def fromString(N: Int, str: String): Board = {
    val cells = for {
      (row, x) <- str.split("\n").filter(!_.isEmpty).zipWithIndex
      (ch, y) <- {
        row.zipWithIndex
      }
    } yield Point(x + 1, y + 1) -> Cell.fromString(ch)
    Board(N, Map(cells.filter(_._2 != EmptyCell): _*))
  }
}

