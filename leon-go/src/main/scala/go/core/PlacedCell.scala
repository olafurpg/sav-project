package go.core

import leon.lang.string
import CellObject._

case class PlacedCell(p: Point, c: Cell) {
  def isValid: Boolean = p.isValid
  def <(that: PlacedCell): Boolean = {
    require(isValid && that.isValid)
    p < that.p
  }

}
