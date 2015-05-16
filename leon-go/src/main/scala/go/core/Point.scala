package go.core

import Point._

case class Point(x: BigInt, y: BigInt) {
  require(isValid(x, y))

  def isValid: Boolean = isValid(x) && isValid(y)

  def isValid(n: BigInt, m: BigInt): Boolean = isValid(n) && isValid(m)

  def isValid(n: BigInt): Boolean = n >= -1 && n < LIMIT

  def <(that: Point): Boolean = {
    require(isValid && that.isValid)
    if (x < that.x) true
    else if (x > that.x) false
    else y < that.y
  }
}


object Point {
  def LIMIT: BigInt = BigInt(10)
  def insideRange(n: BigInt): Boolean = n >= -1 && n < LIMIT
}
