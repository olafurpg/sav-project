package sav.go.leon

import org.scalatest.FunSuite

class BoardTest extends FunSuite {
  val w = WhiteCell
  val b = BlackCell
  val e = WhiteCell

  def dfsTest(lst: List[((Int, Int), Cell)], ignore: Set[((Int, Int), Cell)] = Set.empty): Unit = {
    val b1 = Board(10, Map(lst: _*))
    assert(b1.connectedComponent(1 -> 1 -> b).map(_._1).toList.sorted === lst.filterNot(ignore).map(_._1).sorted)
  }

  def moveTest(n: Int, str1: String, str2: String, c: Cell, p: (Int, Int)): Unit = {
    val b1 = Board.fromString(n, str1.stripMargin)
    val b2 = Board.fromString(n, str2.stripMargin)
    assert(b1.put(c, p) === b2)
  }

  test("dfs should work on basic cases") {
    val ps = List(1 -> 1 -> b, 1 -> 2 -> b)
    val pss = (2 -> 1 -> b) :: ps
    val psss = (2 -> 2 -> w) :: pss
    val bbbb = (2 -> 2 -> b) :: pss
    dfsTest(ps)
    dfsTest(pss)
    dfsTest(psss, Set(2 -> 2 -> w))
    dfsTest(bbbb)
  }

  test("basic put works") {
    val ps = Map(1 -> 1 -> b)
    val b1 = Board(2, ps)
    assert(b1.at(1 -> 1) === BlackCell)
    assert(b1.at(1 -> 2) === EmptyCell)
    assert(b1.at(2 -> 1) === EmptyCell)
    assert(b1.at(2 -> 2) === EmptyCell)
  }

  test("capture works in bug case") {
    val ps = Map(2 -> 2 -> w, 2 -> 3 -> w, 3 -> 2 -> w, 3 -> 3 -> w, 1 -> 2 -> b, 2 -> 1 -> b)
    val b1 = Board(3, ps)
    val b2 = b1.put(BlackCell, 3 -> 1)
    assert(b1.at(3 -> 1) == EmptyCell)
    assert(b2.at(3 -> 1) == BlackCell)
    assert(b2.at(3 -> 2) == WhiteCell)
  }

  test("Board.fromString works") {
    val b1 = Board.fromString(3,
      """
        |xxx
        |ooo
        |
      """.stripMargin)
    val expected: Set[((Int, Int), Cell)] = Set(((1, 1), b), ((1, 2), b), ((1, 3), b), ((2, 1), w), ((2, 2), w), ((2, 3), w))
    val obtained = b1.cells.toSet
    assert(obtained === expected)
    val b2 = Board.fromString(3, "")
    assert(b2.cells.isEmpty)

  }

  test("Simple case 1, no capture") {
    moveTest(5,
      """
        |OX___
        |_____
        |_____
        |_____
        |_____
      """,
      """
      |OX___
      |_O___
      |_____
      |_____
      |_____
    """, w, (2, 2)
    )
  }

}
