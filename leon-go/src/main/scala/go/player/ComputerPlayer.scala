package go.player

import go.core._

case object ComputerPlayer extends Player {
  // TODO: Use smart heuristic to fix depth
  val MaxNodes: Int = 500000

  override def move(g: Game): Step = {
    println(s"AI is thinking...$g")
    val startime = System.currentTimeMillis()
    val depth = 4
    val ai = AI(g.activePlayer)
    val (score, move) = ai.minimax(g, depth)
    val endtime = System.currentTimeMillis()
    val time = endtime - startime
    println(s"Minimax: depth=$depth score=$score runtime=${time}ms nodes=${ai.nodes} nodes/ms=${ai.nodes / time} errors=${ai.errors} nodes/errors=${ai.nodes / ai.errors}")
    move
  }
}

// TODO: game already has activePlayer
case class AI(p: PlayerType) {
  var nodes = 0
  var errors = 0
  def minimax(g: Game, depth: Int = 2): (Int, Step) = {
    nodes += 1
    val maximizingPlayer = g.activePlayer == p
    val mult = if (maximizingPlayer) 1 else -1
    if (depth == 0) {
      val (friend, enemy) = g.state.cells.cells.partition(_.c == g.activePlayer.cell) // TODO: cells.cells --> implement partition method for GoMap
      (friend.size - enemy.size) * (if (maximizingPlayer) 1 else -1) -> Pass
    }
    else {
      g.move(Pass) match {
        case Left(passGame) =>
          val startScore = minimax(passGame, depth - 1)._1
          g.state.freeCells.foldLeft((startScore, Pass: Step)) { case ((currScore, step), p) =>
            val newStep = Place(p.x, p.y)
            g.move(newStep) match {
              case Left(nextGame) =>
                val score = minimax(nextGame, depth - 1)._1
                if (maximizingPlayer && score > currScore) (score, newStep)
                else if (!maximizingPlayer && score < currScore) (score, newStep)
                else (currScore, step)
              case Right(_) => {
                errors += 1
                currScore -> step
              }
            }
          }
        // Pass is illegal for some reason
        case Right(_) => (if (maximizingPlayer) Int.MaxValue else Int.MinValue, Pass)
      }
    }
  }
}