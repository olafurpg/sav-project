package go.core

import go.util.conversions._
import go.collection.GoSet
import go.core.definitions._

object CaptureLogic {
  def capture(board: Board, p: Point, c: Cell): Board = {
    require(board.isValid && !board.isOccupied(p) && board.insideBoard(p))
    val board1 = board.put(c, p)

    // first capture enemy, which may make self dead stones alive
    val captured1 = capturedCells(board1).filterNot(_.c == c)
    val board2 = board1.remove(captured1.map(_.p))

    // capture self dead stones --> for suicide detection
    val captured2 = capturedCells(board2).filter(_.c == c)
    board2.remove(captured2.map(_.p))
  }

  def hasLiberty(board: Board)(p: PlacedCell): Boolean = {
    require(board.isValid)
    board.neighbors(p.p).exists(_.c == EmptyCell)
  }

  def capturedCells(board: Board): GoSet[PlacedCell] = {
    require(board.isValid)

    val e = GoSet.empty[PlacedCell]
    board.cells.foldRight(e -> e) { case (p, (explored, captured)) =>
      if (explored.contains(PlacedCell(p._1, p._2))) explored -> captured
      else {
        val component = connectedComponent(board, PlacedCell(p._1, p._2))

        if (component.exists(hasLiberty(board))) (explored ++ component, captured)
        else (explored ++ component, captured ++ component)
      }
    }._2
  } ensuring(_.isValid)


  def connectedComponent(board: Board, p: PlacedCell, visited: GoSet[PlacedCell] = GoSet.empty): GoSet[PlacedCell] = {
    require(board.isValid && visited.isValid)

    if (visited.contains(p)) visited
    else {
      val newVisited = visited + p
      val toVisit = board.sameColorNeighbors(p)
      toVisit.foldLeft(newVisited) { (b, a) =>
        connectedComponent(board, a, b)
      }
    }
  } ensuring(_.isValid)

}
