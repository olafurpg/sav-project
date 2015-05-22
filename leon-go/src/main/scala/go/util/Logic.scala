package go.util

object Logic {

  def implies(a: Boolean, b: Boolean): Boolean = !a || b

  def iff(a: Boolean, b: Boolean): Boolean = implies(a, b) && implies(b, a)

  def ifThenElse(cond: Boolean, thenCond: Boolean, elseCond: Boolean): Boolean  =
    (cond && thenCond) || (!cond && elseCond)

}
