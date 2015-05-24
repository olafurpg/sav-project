package go.core

import go.util.conversions._
import go.util.Logic._
import go.collection.GoSet
import go.core.definitions._
import leon.annotation.library
import leon.lang._
import leon.collection._
import leon.annotation._

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

      if (!component.exists(hasLiberty(board)))
        capturedCellsRecursive(board, toVisit.tail, GoSet(captured.elements ++ component))
      else
        capturedCellsRecursive(board, toVisit.tail, captured)
    }
  } ensuring { res =>
    res.forall(board.isOnBoard) &&
      board.cells.pairs.map(tpl2PlacedCell).forall { x =>
        implies(hasLiberty(board)(x), !res.contains(x))
        // Leon can't detect this bug
        //         implies(!hasLiberty(board)(x), res.contains(x))
      }
  }

  def connectedComponent(board: Board, pc: PlacedCell): List[PlacedCell] = {
    require(board.isValid && board.isOnBoard(pc))
    connectedComponentRecursive(board, pc.c, List(pc))
  } ensuring { res =>
    res.contains(pc)
  }

  def connectedComponentRecursive(board: Board, color: Cell, toVisit: List[PlacedCell], component: List[PlacedCell] = List[PlacedCell]()): List[PlacedCell] = {
    require(board.isValid &&
      board.isValidList(toVisit) &&
      board.isValidList(component)
    )
    if (toVisit.isEmpty) component
    else if (component.contains(toVisit.head)) connectedComponentRecursive(board, color, toVisit.tail, component)
    else {
      val p = toVisit.head
      val newComponent = addElement(board, component, p)
      val newNeighbors = board.neighbors(p.p).filter(_.c == color)
      val newToVisit = addElements(board, toVisit, newNeighbors)
      connectedComponentRecursive(board, color, newToVisit, newComponent)
    }
  } ensuring { res =>
    board.isValidList(res)
  }

  def ccColor1(board: Board, toVisit: PlacedCell): Boolean = {
    require(
      board.isValid &&
        board.isOnBoard(toVisit)
    )
    connectedComponentRecursive(board, toVisit.c, List(toVisit)).forall(_.c == toVisit.c)
  }.holds

  def ccContainsRoot(board: Board, root: PlacedCell): Boolean = {
    require(
      board.isValid &&
        board.isOnBoard(root)
    )
    connectedComponentRecursive(board, root.c, List(root)).contains(root)
  }.holds

  def addElement(board: Board, lst: List[PlacedCell], e: PlacedCell): List[PlacedCell] = {
    require(
      board.isValid &&
        board.isValidList(lst) &&
        board.isOnBoard(e)
    )
    e :: lst
  } ensuring (board.isValidList(_))

  def addColoredElements(color: Cell, board: Board, a: List[PlacedCell], b: List[PlacedCell]): List[PlacedCell] = {
    require(
      a.forall(_.c == color) &&
        b.forall(_.c == color) &&
        board.isValid &&
        board.isValidList(a) &&
        board.isValidList(b)
    )
    if (a.isEmpty) b
    else addColoredElements(color, board, a.tail, a.head :: b)
  } ensuring { res =>
    board.isValidList(res) &&
      res.forall(_.c == color)
  }

  def addElements(board: Board, a: List[PlacedCell], b: List[PlacedCell]): List[PlacedCell] = {
    require(
      board.isValid &&
        board.isValidList(a) &&
        board.isValidList(b)
    )
    if (a.isEmpty) b
    else addElements(board, a.tail, a.head :: b)
  } ensuring { res =>
    board.isValidList(res)
  }

  def connectedComponentOfEmptyBoardIsEmpty(): Boolean = {
    val b = Board.empty(BigInt(3))
    val cells = b.cells.map(tpl2PlacedCell)
    //    connectedComponent(Board.empty(3), )
    true
  }

}
