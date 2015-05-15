package go.player

import go.core._
import go.util.StringUtil
import scala.io.StdIn

case object HumanPlayer extends Player with StringUtil {
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
        Rule.check(g, p) match {
          case Some(OutsideOfBoardError) =>
            println(s"$p is out of range, try again")
            readStep(g)
          case Some(AlreadyOccupiedError) =>
            println(s"$p is already taken, try again")
            readStep(g)
          case Some(SuicideError) =>
            println(s"$p is a suicide move, try again")
            readStep(g)
          case Some(KoError) =>
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
