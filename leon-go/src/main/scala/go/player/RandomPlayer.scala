package go.player

import scala.util.Random
import go.core._
import go.core.definitions._
import go.collection._

case object RandomPlayer extends Player {
  private val random = new Random

  def name = "Bob(stupid)"

  override def move(g: Game): Step = {
    if (g.state.full) Pass
    else {
      val list = g.state.freeCells.elements
      val Point(x, y) = list(random.nextInt(list.length))
      Place(x, y)
    }
  }
}
