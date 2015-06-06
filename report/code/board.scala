case class Board(n: BigInt, cells: GoMap[Point, Cell])

case class Game(states: List[Board], steps: List[Step], activePlayer: PlayerType, size: BigInt)
