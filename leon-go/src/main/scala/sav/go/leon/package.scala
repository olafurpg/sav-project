package sav.go

package object leon {
//  type Point = (Int, Int)
  type E = (Point, Cell)
  implicit def tpl2Point(tpl: (Int, Int)): Point = Point(tpl._1, tpl._2)

  implicit def tpl2PlacedCell(tpl: (Point, Cell)): PlacedCell = PlacedCell(tpl._1, tpl._2)

}
