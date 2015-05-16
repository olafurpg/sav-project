package go.core

import go.util.conversions._
import go.collection.GoSet
import CellObject._

object CaptureLogic {
  def put(b: Board, p: Point, c: Cell): Board = {

    val pc = PlacedCell(p, c)

    val board1 = b.put(c, p)
    val captured1 = capturedCells(board1).filterNot(_.c == c)

    val board2 = board1.remove(captured1)
    val captured2 = capturedCells(board2).filter(_.c == c)

    board2.remove(captured2)
  }

  def hasLiberty(b: Board)(p: PlacedCell): Boolean = b.neighbors(p.p).exists(_.c == EmptyCell)

  def capturedCells(b: Board): GoSet = {
    //    println(s"this = $this")
    val e = GoSet.empty
    b.cells.foldRight(e -> e) {
      case (p, (explored, captured)) =>
        if (explored.contains(p)) explored -> captured
        else {
          val component = connectedComponent(b, p)
          //          println(s"p = $p, component = $component, explored = $explored, captured = $captured")
          if (component.exists(hasLiberty(b))) (explored ++ component, captured)
          else (explored ++ component, captured ++ component)
        }
    }._2
  }


  def connectedComponent(board: Board, p: PlacedCell, visited: GoSet = GoSet.empty): GoSet = {
    if (visited.contains(p)) visited
    else {

      val newVisited = visited + p
      val toVisit = board.sameColorNeighbors(p)
      toVisit.foldLeft(newVisited) {
        case (b, a) =>
          connectedComponent(board, a, b)
      }
    }
  }

}
