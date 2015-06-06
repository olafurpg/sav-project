def addValidElements(board: Board,
    a: List[Point], b: List[Point]): List[Point] = {
  require( board.isValid && board.isValidPoints(a) && board.isValidPoints(b))
  if (a.isEmpty) b
  else addValidElements(board, a.tail, a.head :: b)
} ensuring { res =>
  board.isValidPoints(res) && res.content == (a ++ b).content
}
