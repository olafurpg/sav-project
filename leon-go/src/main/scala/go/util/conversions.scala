package go.util

import go.core.{Point, PlacedCell, Cell}

package object conversions {
//  type Point = (Int, Int)
  implicit def tpl2Point(tpl: (Int, Int)): Point = Point(tpl._1, tpl._2)

  implicit def tpl2PlacedCell(tpl: (Point, Cell)): PlacedCell = PlacedCell(tpl._1, tpl._2)
}
