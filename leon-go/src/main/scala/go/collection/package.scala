package go
import leon.collection._

package object collection {
  def mkString[T](lst: List[T], sep: String): String = lst.mkString(sep)
  def mkString[T](lst: List[T], start: String, sep: String, end: String): String = lst.mkString(start, sep, end)

}
