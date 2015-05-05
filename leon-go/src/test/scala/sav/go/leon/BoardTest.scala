package sav.go.leon

import org.scalatest.FunSuite

class BoardTest extends FunSuite {
  val w = WhiteCell
  val b = BlackCell

  def dfsTest(lst: List[((Int, Int), Cell)], ignore: Set[((Int, Int), Cell)] = Set.empty): Unit = {
    val b1 = Board(10, Map(lst:_*))
    assert(b1.dfs(1 -> 1).toList.sorted === lst.filterNot(ignore).map(_._1).sorted)
  }

  test("dfs should work on basic cases") {
    val ps = List(1 -> 1 -> b, 1 -> 2 -> b)
    val pss = (2 -> 1 -> b) :: ps
    val psss = (2 -> 2 -> w) :: pss
    dfsTest(ps)
    dfsTest(pss)
    dfsTest(psss, Set(2 -> 2 -> w))
  }

// TODO: Bug
//  *************
//  * Round 14
//  * BlackPlayer
//    * Size: 5 x 5
//  *************
//  X_X__
//  _XX__
//  _X_X_
//  __XOO
//  ___OO
//  x y to place, p to pass, q to quit: 5 3
//  *************
//  * Round 15
//  * WhitePlayer
//    * Size: 5 x 5
//  *************
//  X_X__
//  _XX__
//  _X_X_
//  __XOO
//  __X_O


}
