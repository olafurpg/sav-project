package go.core

sealed abstract class Step

case object Pass extends Step
case class Place(x: Int, y: Int) extends Step

object Step {
  def isValid(s: Step, b: Board): Boolean = {
    s match {
      case Pass => true
      case Place(x, y) => b.at(x, y) == EmptyCell
    }
  } ensuring { res =>
    if (s == Pass) true
    else {
      val Place(x, y) = s
      res == (b.at(x, y) == EmptyCell)
    }
  }
}
