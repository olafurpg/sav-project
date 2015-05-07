package sav.go.scala

import sav.go.leon._

import scala.io.StdIn

trait Player {
  def move(g: Game): Step
}



case object HumanPlayer extends Player {
  import Util._
  val coordinate = """\s*(\d+) (\d+)\s*""".r

  // String  => Either[Step, Error]
  // UIEvent => Either[Step, Error]
  def readStep(g: Game): Step = {
    StdIn.readLine("x y to place, p to pass, q to quit: ") match {
      case "p" => Pass
      case "q" => {
        println(s"Goodbye!")
        System.exit(0)
        ???
      }
      case coordinate(xStr, yStr) => {
        val p = Place(xStr.toInt, yStr.toInt)
        g.move(p) match {
          case Right(OutsideOfBoardError) =>
            println(s"$p is out of range, try again")
            readStep(g)
          case Right(AlreadyOccupiedError) =>
            println(s"$p is already taken, try again")
            readStep(g)
          case Right(SuicideError) =>
            println(s"$p is a suicide move, try again")
            readStep(g)
          case Right(KoError) =>
            println(s"$p breaks the Ko rule, try again")
            readStep(g)
          case _ => p
        }
      }
      case s => {
        println(s"Input '$s' is illegal, try again")
        readStep(g)
      }
    }
  }
  override def move(g: Game): Step = {
    println(message("Round " + g.round, g.activePlayer.toString, s"Size: ${g.size} x ${g.size}"))
    println(g)
    readStep(g)
  }
}

case object ComputerPlayer extends Player {
  override def move(g: Game): Step = AI(g.activePlayer).minimax(g, 4)._2
}

case class AI(p: PlayerType) {
  var nodes = 0
  def minimax(g: Game, depth: Int = 2): (Int, Step) = {
    nodes++
    val maximizingPlayer = g.activePlayer == p
    val mult = if (maximizingPlayer) 1 else -1
    if (depth == 0) {
      val (friend, enemy) = g.state.cells.partition(_.c == g.activePlayer.cell)
      (friend.size - enemy.size) * (if (maximizingPlayer) 1 else -1) -> Pass
    }
    else {
      g.move(Pass) match {
        case Left(passGame) =>
          val startScore = minimax(passGame, depth - 1)._1
          g.state.freeCells.par.foldLeft((startScore, Pass: Step)) { case ((currScore, step), p) =>
            val newStep = Place(p.x, p.y)
            g.move(newStep) match {
              case Left(nextGame) =>
                val score = minimax(nextGame, depth - 1)._1
                if (maximizingPlayer && score > currScore) (score, newStep)
                else if (!maximizingPlayer && score < currScore) (score, newStep)
                else (currScore, step)
              case Right(_) => currScore -> step
            }
          }
        // Pass is illegal for some reason
        case Right(_) => (if (maximizingPlayer) Int.MaxValue else Int.MinValue, Pass)
      }
    }
  }
}

