package sav.go.leon

case class Board(n: Int, cells: Map[(Int, Int), Cell]) {
  def this(n: Int) = this(n, Map.empty)

  def inRange(x: Int) = 0 < x && x <= n

  def insideBoard(p: (Int, Int)) = inRange(p._1) && inRange(p._2)

  def at(x: Int, y: Int): Cell = cells.getOrElse((x, y), EmptyCell)

  def at(p: (Int, Int)): Cell = at(p._1, p._2)

  val r = 1 to n

  def board = {
    r.map(x => r.map(y => at(x, y)))
  }

  // TODO: disallow suicide
  def put(c: Cell, p: (Int, Int)): Board = {
    require(insideBoard(p) && !cells.contains(p))
    Board(n, cells.filter(x => at(x._1) == c.otherColor && isCaptured(p)(x)) + (p -> c))
  }

  // TODO: take into account color
  def isCaptured(newP: (Int, Int))(p: ((Int, Int), Cell)): Boolean = {
    dfs(p._1).exists(p => !emptyNeighors(p).filter(_ != newP).isEmpty)
  }

  def hasLiberty(p: (Int, Int)): Boolean = neighboors(p).map(at).contains(EmptyCell)

  def neighboors(p: (Int, Int)): List[(Int, Int)] =
    neighboors(p._1, p._2)

  def sameColorNeighbors(p: (Int, Int)): List[(Int, Int)] = {
    val c = at(p)
    neighboors(p).filter(x => at(x) == c)
  }

  def emptyNeighors(p: (Int, Int)): List[(Int, Int)] = {
    neighboors(p).filter(x => at(x) == EmptyCell)
  }

  def neighboors(x: Int, y: Int): List[(Int, Int)] =
    List((x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)).filter(insideBoard)

  def dfs(p: (Int, Int)): Set[(Int, Int)] = dfs(p, Set.empty)

  def dfs(p: (Int, Int), visited: Set[(Int, Int)]): Set[(Int, Int)] = {
    if (visited.contains(p)) visited
    else {
      val newVisited = visited + p
      val toVisit = sameColorNeighbors(p)
      toVisit.foldRight(newVisited) { case (a, b) =>
        //        println(s"p = $p, toVisit = $toVisit, a = $a, b = $b")
        dfs(a, b)
      }
    }
  }

  def full: Boolean = cells.size == n * n

  def playerCells(p: PlayerType): Set[(Int, Int)] = cells.withFilter(_._2 == p.cell).map(_._1).toSet

  def next(m: Step, c: Cell): Board = m match {
    case Pass => Board(n, cells)
    case Place(x, y) => put(c, x -> y)
  }

  override def toString() = {
    board.map(_.mkString("")).mkString("\n")
  }
}

object Board {

  def fromString(N: Int, str: String): Board = {
    val cells = for {
      (row, x) <- str.split("\n").filter(!_.isEmpty).zipWithIndex
      (ch, y) <- {
        println(s"$row")
        row.zipWithIndex
      }
    } yield (x + 1) -> (y + 1) -> Cell.fromString(ch)
    println(cells.toList)
    Board(N, Map(cells.filter(_._2 != EmptyCell):_*))
  }
}

