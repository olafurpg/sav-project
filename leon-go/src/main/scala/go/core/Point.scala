package go.core

import leon.lang.string

case class Point(x: Int, y: Int) {
  def tpl: (Int, Int) = x -> y
}
