package go.core

case class Point(x: Int, y: Int) {
  def <(that: Point): Boolean = {
    if (x < that.x) true
    else if (x > that.x) false
    else y < that.y
  }
}


