package go.core

case class Point(x: Int, y: Int) {
  require(insideRange(x, y))

  def insideRange: Boolean = insideRange(x) && insideRange(y)

  def insideRange(n: Int, m: Int): Boolean = insideRange(n) && insideRange(m)

  def insideRange(n: Int): Boolean = n >= -1 && n < 10

  def <(that: Point): Boolean = {
    require(insideRange && that.insideRange)
    if (x < that.x) true
    else if (x > that.x) false
    else y < that.y
  }
}


