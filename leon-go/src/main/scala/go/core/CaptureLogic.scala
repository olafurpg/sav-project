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
      val component = connectedComponentRecursive(board, toVisit.head.c, List(toVisit.head.p))
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

  def connectedComponent(board: Board, pc: PlacedCell): List[Point] = {
    require(board.isValid && board.isOnBoard(pc))
    connectedComponentRecursive(board, pc.c, List(pc.p))
  } ensuring { res =>
    res.contains(pc.p)
  }

  def connectedComponentRecursive(board: Board, color: Cell, toVisit: List[Point], component: List[Point] = List[Point]()): List[Point] = {
    require(board.isValid &&
      board.isValidPoints(toVisit) &&
      board.isValidPoints(component) &&
      isComponent(board, component)
    )
    if (toVisit.isEmpty)
      component
    else if (component.contains(toVisit.head))
      connectedComponentRecursive(board, color, toVisit.tail, component)
    else {
      val p = toVisit.head
      val newComponent = addElement(board, component, p)
      val newNeighbors = board.sameColorNeighborPoints(p, color)
      val newToVisit = addElements(board, toVisit, newNeighbors)
      connectedComponentRecursive(board, color, newToVisit, newComponent)
    }
  } ensuring { res =>
    isComponent(board, res)
  }

  def isComponent(board: Board, lst: List[Point]): Boolean = {
    require(board.isValid && board.isValidPoints(lst))
    if (lst.isEmpty) true
    else
      true
    // lst.tail.forall(x => isConnected(lst.head, x))
  }

  def addElement(board: Board, lst: List[Point], e: Point): List[Point] = {
    require(
      board.isValid &&
        isComponent(board, lst) &&
        board.isValidPoints(lst) &&
        board.insideBoard(e)
    )

    e :: lst
  } ensuring { res =>
    isComponent(board, res)
  }

  def addElements(board: Board, a: List[Point], b: List[Point]): List[Point] = {
    require(
        board.isValid &&
        board.isValidPoints(a) &&
        board.isValidPoints(b)
    )
    if (a.isEmpty) b
    else addElements(board, a.tail, a.head :: b)
  } ensuring { res =>
    isComponent(board, res)
  }
}
