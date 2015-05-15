package go.core

sealed trait Step {
  def isValid(b: Board): Boolean = this match {
    case Pass => true
    case Place(x, y) => b.at(x, y) == EmptyCell
  }
}

case object Pass extends Step
case class Place(x: Int, y: Int) extends Step
