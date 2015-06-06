def connectedComponent(
    board: Board, color: Cell,
    toVisit: List[Point], component: List[Point]): List[Point] = {
  if (toVisit.isEmpty)
    component
  else if (component.contains(toVisit.head))
    connectedComponent(board, color, toVisit.tail, component)
  else {
    val p = toVisit.head
    val newComponent = p :: component
    val newNeighbors = board.sameColorNeighborPoints(p, color)
    val newToVisit = toVisit.tail ++ newNeighbors
    connectedComponent(board, color, newToVisit, newComponent)
  }
}
