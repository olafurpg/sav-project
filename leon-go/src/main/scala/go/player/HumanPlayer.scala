package go.player

import go.core._
import scala.io.StdIn

case object HumanPlayer extends Player {
  import go.util.Util._
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