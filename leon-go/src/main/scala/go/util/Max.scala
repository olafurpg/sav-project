package go.util

object Max {
  def max(x: BigInt, y: BigInt): BigInt = {
    if (x <= y) y
    else x
  } ensuring(res => x <= res && y <= res)
}
