package go.util

import go.core.definitions._
import leon.lang.string

package object conversions {
  def bigIntTuple2Point(tpl: (BigInt, BigInt)): Point = Point(tpl._1, tpl._2)

  def tpl2PlacedCell(tpl: (Point, Cell)): PlacedCell = PlacedCell(tpl._1, tpl._2)
}
