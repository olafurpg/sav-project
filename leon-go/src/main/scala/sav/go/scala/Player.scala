package sav.go.scala

import sav.go.leon.{Pass, Step, Game, Place}

import scala.io.StdIn

trait Player {
  def move(g: Game): Step
}



case object HumanPlayer extends Player with Util {
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
        val (x, y) = (xStr.toInt, yStr.toInt)
        if (!g.state.inRange(x) || !g.state.inRange(y)) {
          // Error => IO
          println(s"Point ($x, $y) is out of range, try again")
          readStep(g)
        }
        else if (g.state.cells.contains((x, y))) {
          println(s"Point ($x, $y) is already taken, try again")
          readStep(g)
        }
        else {
          Place(x, y)
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
  override def move(g: Game): Step = Place(0, 0)
}

