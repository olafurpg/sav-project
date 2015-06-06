case class Point(x: BigInt, y: BigInt) {
  require(x > 0 && y > 0)
  def +(that: Point): Point = Point(x + that.x, y + that.y)
}
