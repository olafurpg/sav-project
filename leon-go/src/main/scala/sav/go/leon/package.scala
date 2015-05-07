package sav.go

package object leon {
//  type Point = (Int, Int)
  implicit def tpl2Point(tpl: (Int, Int)): Point = Point(tpl._1, tpl._2)

}
