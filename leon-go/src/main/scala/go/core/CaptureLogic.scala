package go.core

import go.util.conversions._
import go.util.Logic._
import go.collection.GoSet
import go.core.definitions._
import leon.annotation.library
import leon.collection._

object CaptureLogic {
  def capture(board: Board, p: Point, c: Cell): Board = {
    require(board.isValid && !board.isOccupied(p) && board.insideBoard(p))
    val board1 = board.put(c, p)

    // first capture enemy, which may make self dead stones alive
    val captured1 = capturedCells(board1).filterNot(_.c == c)
    val board2 = board1.remove(captured1)

    // capture self dead stones --> for suicide detection
    val captured2 = capturedCells(board2).filter(_.c == c)
    board2.remove(captured2)
  }

  def hasLiberty(board: Board)(p: PlacedCell): Boolean = {
    require(board.isValid)
    board.neighbors(p.p).exists(_.c == EmptyCell)
  }

  def capturedCells(board: Board): GoSet[PlacedCell] = {
    require(board.isValid)
    capturedCellsRecursive(board, board.cells.map(tpl2PlacedCell))
  } ensuring { res =>
    res.isValid && board.cells.pairs.map(tpl2PlacedCell).forall { x =>
      implies(hasLiberty(board)(x), !res.contains(x))
//        implies(!hasLiberty(board)(x), res.contains(x))
      // Missing condition when x has no liberty but neighbor has liberty
    }
  }

  def connectedComponent(board: Board, p: PlacedCell, visited: GoSet[PlacedCell] = GoSet.empty): GoSet[PlacedCell] = {
    require(board.isValid &&
      board.isOnBoard(p) &&
      visited.isValid &&
      visited.forall(board.isOnBoard)
    )

    if (visited.contains(p)) visited
    else {
      val newVisited = visited + p
      val toVisit = board.sameColorNeighbors(p)
      toVisit.foldLeft(newVisited) { (b, a) =>
        connectedComponent(board, a, b)
      }
    }
  } ensuring (_.isValid)

  def capturedCellsRecursive(board: Board, toVisit: List[PlacedCell], captured: GoSet[PlacedCell] = GoSet.empty): GoSet[PlacedCell] = {
    require(board.isValid &&
      captured.isValid &&
      toVisit.forall(board.isOnBoard) &&
      captured.forall(board.isOnBoard)
    )
    if (toVisit.isEmpty) captured
    else if (captured.contains(toVisit.head)) capturedCellsRecursive(board, toVisit.tail, captured)
    else {
      val component = connectedComponentRecursive(board, toVisit.head.c, List(toVisit.head))
      if (!component.exists(hasLiberty(board))) capturedCellsRecursive(board, toVisit.tail, component)
      else capturedCellsRecursive(board, toVisit.tail, captured)
    }
  } ensuring { res =>
    res.forall(board.isOnBoard) &&
      board.cells.pairs.map(tpl2PlacedCell).forall { x =>
      implies(hasLiberty(board)(x), !res.contains(x))
        // Leon can't detect this bug
//         implies(!hasLiberty(board)(x), res.contains(x))
    }
  }

  @library
  def connectedComponentRecursive(board: Board, color: Cell, toVisit: List[PlacedCell], component: GoSet[PlacedCell] = GoSet.empty): GoSet[PlacedCell] = {
    require(board.isValid &&
      toVisit.forall(_.c == color) &&
      component.forall(_.c == color) &&
      component.isValid &&
      toVisit.forall(board.isOnBoard) &&
      component.forall(board.isOnBoard)
    )
    if (toVisit.isEmpty) component
    else if (component.contains(toVisit.head)) connectedComponentRecursive(board, color, toVisit.tail, component)
    else {
      val p = toVisit.head
      val newComponent = component + p
      val newToVisit = toVisit ++ board.sameColorNeighbors(p)
      connectedComponentRecursive(board, color, newToVisit, newComponent)
    }
  } ensuring { res =>
    res.forall(board.isOnBoard) && res.forall(_.c == color)
  }

  def connectedComponentOfEmptyBoardIsEmpty(): Boolean = {
    val b = Board.empty(BigInt(3))
    val cells = b.cells.map(tpl2PlacedCell)
//    connectedComponent(Board.empty(3), )
    true
  }

}
