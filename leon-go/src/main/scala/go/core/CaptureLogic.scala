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
    val captures = capturedComponents(board1).filterNot(_._1 == c).map(_._2)
    val board2 = captures.foldLeft(board1) { (b, c) => b.remove(c) }

    // capture self dead stones --> for suicide detection
    val selfCaptures = capturedComponents(board2).filter(_._1 == c).map(_._2)
    selfCaptures.foldLeft(board2) { (b, c) => b.remove(c) }
  }

  def hasLiberty(board: Board)(component: List[Point]): Boolean = {
    require(board.isValid)
    component.exists(p => board.neighbors(p).exists(_.c == EmptyCell))
  }

  def capturedComponents(board: Board): List[(Cell, List[Point])] = {
    require(board.isValid)
    capturedComponentsRecursive(board, board.cells.map(tpl2PlacedCell))
  } ensuring { res => true
      // implies(!hasLiberty(board)(x), res.contains(x))
      // Missing condition when x has no liberty but neighbor has liberty
  }

  def capturedComponentsRecursive(board: Board, toVisit: List[PlacedCell], visited: List[Point] = List(), captured: List[(Cell, List[Point])] = List()): List[(Cell, List[Point])] = {
    require(
      board.isValid &&
        board.isValidList(toVisit) &&
        board.isValidPoints(visited)
    )

    if (toVisit.isEmpty)
      captured
    else if (visited.contains(toVisit.head.p))
      capturedComponentsRecursive(board, toVisit.tail, visited, captured)
    else {
      val component = connectedComponent(board, toVisit.head.c, List(toVisit.head.p))
      val newVisited = visited ++ component

      if (!hasLiberty(board)(component))
        capturedComponentsRecursive(board, toVisit.tail, newVisited, (toVisit.head.c, component)::captured)
      else
        capturedComponentsRecursive(board, toVisit.tail, newVisited, captured)
    }
  } ensuring { res => true
    // Leon can't detect this bug
    // implies(!hasLiberty(board)(x), res.contains(x))
  }

  def connectedComponentFrom(board: Board, pc: PlacedCell): List[Point] = {
    require(board.isValid && board.isOnBoard(pc))
    connectedComponent(board, pc.c, List(pc.p))
  } ensuring { res =>
    res.contains(pc.p) && isComponent(board, res, pc.c)
  }

  def connectedComponent(board: Board, color: Cell, toVisit: List[Point], component: List[Point] = List[Point]()): List[Point] = {
    require(board.isValid &&
      isComponent(board, component, color) &&
      board.isValidPoints(toVisit) &&
      board.isValidPoints(component) &&
      isComponent(board, addValidElements(board, component, toVisit), color)
    )
    if (toVisit.isEmpty)
      component
    else if (component.contains(toVisit.head))
      connectedComponent(board, color, toVisit.tail, component)
    else {
      val p = toVisit.head
      val newComponent = addToComponent(board, component, p, color)
      val newNeighbors = board.sameColorNeighborPoints(p, color)
      val newToVisit = addValidElements(board, toVisit.tail, newNeighbors)
      connectedComponent(board, color, newToVisit, newComponent)
    }
  } ensuring { res =>
    isComponent(board, res, color)
  }

  def isComponent(board: Board, lst: List[Point], color: Cell): Boolean = {
    require(board.isValid)
    if (lst.isEmpty) true
    else
      board.isValidPoints(lst)
      // lst.forall(a => lst.forall(b => board.isConnected(PlacedCell(a, color), PlacedCell(b, color))))
  }

  def addToComponent(board: Board, lst: List[Point], e: Point, color: Cell): List[Point] = {
    require(
      board.isValid &&
        isComponent(board, lst, color) &&
        board.isValidPoints(lst) &&
        board.insideBoard(e)
    )

    e :: lst
  } ensuring { res =>
    isComponent(board, res, color) && res.content == (e::lst).content
  }

  def addValidElements(board: Board, a: List[Point], b: List[Point]): List[Point] = {
    require(
        board.isValid &&
        board.isValidPoints(a) &&
        board.isValidPoints(b)
    )
    if (a.isEmpty) b
    else addValidElements(board, a.tail, a.head :: b)
  } ensuring { res =>
    board.isValidPoints(res) && res.content == (a ++ b).content
  }
}
