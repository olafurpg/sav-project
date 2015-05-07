package sav.go.leon

import org.scalatest.FunSuite

class BoardTest extends FunSuite {
  val w = WhiteCell
  val b = BlackCell
  val e = WhiteCell

  def dfsTest(lst: List[(Point, Cell)], ignore: Set[(Point, Cell)] = Set.empty): Unit = {
    val b1 = Board(10, Map(lst: _*))
    assert(b1.connectedComponent(PlacedCell(Point(1, 1), b)).map(_.p).toList.sortBy(_.tpl) === lst.filterNot(ignore).map(_._1).sortBy(_.tpl))
  }

  def moveTest(n: Int, str1: String, str2: String, c: Cell, p: Point): Unit = {
    val b1 = Board.fromString(n, str1.stripMargin)
    val b2 = Board.fromString(n, str2.stripMargin)
    assert(b1.put(c, p) === b2)
  }

  test("dfs should work on basic cases") {
    val ps = List(Point(1, 1) -> b, Point(1, 2) -> b)
    val pss = Point(2, 1) -> b :: ps
    val psss = Point(2, 2) -> w :: pss
    val bbbb = Point(2, 2) -> b :: pss
    dfsTest(ps)
    dfsTest(pss)
    dfsTest(psss, Set(Point(2, 2) -> w))
    dfsTest(bbbb)
  }

  test("basic put works") {
    val ps = Map(Point(1, 1) -> b)
    val b1 = Board(2, ps)
    assert(b1.at(1 -> 1) === BlackCell)
    assert(b1.at(1 -> 2) === EmptyCell)
    assert(b1.at(2 -> 1) === EmptyCell)
    assert(b1.at(2 -> 2) === EmptyCell)
  }

  test("capture works in bug case") {
    val ps = Map(Point(2, 2) -> w, Point(2, 3) -> w, Point(3, 2) -> w, Point(3, 3) -> w, Point(1, 2) -> b, Point(2, 1) -> b)
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
    val expected: Set[(Point, Cell)] = Set((Point(1, 1), b), (Point(1, 2), b), (Point(1, 3), b), (Point(2, 1), w), (Point(2, 2), w), (Point(2, 3), w))
    val obtained = b1.cells.toSet
    assert(obtained === expected)
    val b2 = Board.fromString(3, "")
    assert(b2.cells.isEmpty)

  }

  test("capturedCells works") {
    moveTest(5,
      """
        |OX___
        |_O___
        |_____
        |_____
        |_____
      """,
      """
        |O O__
        |_O___
        |_____
        |_____
        |_____
      """, w, 1 -> 3
    )

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
  test("Board.put suicide is captured") {
    moveTest(5,
      """
        |X_X__
        |_X_X_
        |_XXX_
        |__X__
        |_____
      """,
      """
        |X_X__
        |_X_X_
        |_XXX_
        |__X__
        |_____
      """, w, (2, 3)
    )
  }

}
