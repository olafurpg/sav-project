
import leon.lang._
import leon.annotation._
import leon.lang.string.String
import leon.collection._

case class GoMap(cells: List[Int]) {
  require(isSorted(cells))

  def insSort(lst: List[Int], v: Int): List[Int] = {
    require(isSorted(lst))
    lst match {
      case l if l.isEmpty => List(v)
      case _ =>
        if (v < lst.head) {
          v :: lst
        } else if (v == lst.head) {
          lst
        } else {
          lst.head :: insSort(lst.tail, v)
        }
    }
  } ensuring(res => {
    isSorted(res)
  })
  
  def isSorted(lst: List[Int]): Boolean = lst match {
    case l if l.size <= 1 => true
    case _ =>
      if (lst.tail.head < lst.head) false
      else isSorted(lst.tail)
  }

  def +(e: Int): GoMap = {
    // require(isSorted(cells))
    GoMap(insSort(cells, e))
  }
  
}
