package sav.go.leon

// TODO: Add rule engine
case class Game(states: List[Board], steps: List[Step]) {
  //  require(s.length > 0)

  def state: Board = {
    require(states.length > 0)
    states.head
  }

  val activePlayer: PlayerType = if (states.length % 2 == 1) BlackPlayer else WhitePlayer

  val round: Int = states.length - 1

  val size: Int = state.n

  def isOver: Boolean = {
    round > 1 && steps.head == Pass && steps.tail.head == Pass
  }

  def move(m: Step): Game = {

    Game(state.next(m, activePlayer.cell) :: states, m :: steps)
  }

  override def toString(): String = state.toString()
}

object Game {
  def apply(b: Board): Game = Game(List(b), Nil)
  def apply(n: Int): Game = Game(new Board(n))
}


