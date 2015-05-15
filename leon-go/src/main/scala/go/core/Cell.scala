package go.core

// TODO: rename to stone
object Cell {
  def fromString(ch: Char): Cell = ch match {
    case 'X' | 'x' => BlackCell
    case 'O' | 'o' => WhiteCell
    case _ => EmptyCell
  }
}
sealed abstract class Cell {
  def otherColor: Cell = this match {
    case WhiteCell => BlackCell
    case BlackCell => WhiteCell
    case EmptyCell => EmptyCell
  }
}

case object WhiteCell extends Cell {
  override def toString(): String = "O"
}
case object BlackCell extends Cell {
  override def toString(): String = "X"
}
case object EmptyCell extends Cell {
  override def toString(): String = "_"
}
