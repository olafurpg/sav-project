package sav.go.leon

object Max {
  def max(x: BigInt, y: BigInt): BigInt = {
    if (x <= y) x
    else x
  } ensuring(res => x <= res && y <= res)
}

