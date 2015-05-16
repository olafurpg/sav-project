package go.util

object LogicUtil {

  def implies(a: Boolean, b: Boolean): Boolean = !a || b

}
