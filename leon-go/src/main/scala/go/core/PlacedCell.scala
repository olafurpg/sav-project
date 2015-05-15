package go.core

import leon.lang.string
import CellObject._

case class PlacedCell(p: Point, c: Cell) {
  def <(that: PlacedCell): Boolean = p < that.p
}
