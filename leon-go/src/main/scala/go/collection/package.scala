package go

import leon.collection._
import leon.lang.string

package object collection {
  def mkString[T](lst: List[T], sep: String): String = mkString(lst, "", sep, "")

  def mkString[T](lst: List[T], start: String, sep: String, end: String): String = lst match {
    case l if l.size == 0 => ""
    case l if l.size == 1 => start + l.head.toString + end
    case l if l.size > 1 => start + l.head.toString + sep + mkString(l.tail, "", sep, "") + end
  }
}
