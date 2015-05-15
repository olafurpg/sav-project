package sav.go.leon

import go.util.StringUtil
import org.scalatest.FunSuite
import leon.collection._
import go.collection._
import go.core._
import go.util.conversions._
import CellObject._

trait Util extends FunSuite with StringUtil {
  val w = WhiteCell
  val b = BlackCell
  val e = WhiteCell

  def fromString(N: Int, str: String): Board = {
    val cells = for {
      (row, x) <- str.stripMargin.split("\n").filter(!_.isEmpty).zipWithIndex
      (ch, y) <- {
        row.zipWithIndex
      }
    } yield PlacedCell(Point(x + 1, y + 1), cellFromString(ch))
    Board(N, GoMap(cells.filter(_.c != EmptyCell).toList))
  }

  def dfsTest(lst: List[PlacedCell], ignore: Set[PlacedCell] = Set.empty): Unit = {
    val expected = GoSet(lst.filterNot(ignore))
    val b1 = Board(10, GoMap(lst))
    assert(CaptureLogic.connectedComponent(b1, PlacedCell(Point(1, 1), b)).isEqualTo(expected))
  }

  def moveTest(n: Int, str1: String, str2: String, c: Cell, p: Point): Unit = {
    val b1 = fromString(n, str1.stripMargin)
    val b2 = fromString(n, str2.stripMargin)
    assert(CaptureLogic.put(p, c, b1) === b2)
  }

  val B1 = fromString(4,
    """
      |_XO_
      |XO_O
      |_XO_
      |____
    """)

  val B2 = fromString(4,
    """
      |_XO_
      |X_XO
      |_XO_
      |____
    """)

  val B3 = fromString(5,
    """
      |XO_OO
      |_XO_O
      |__XO_
      |_X_X_
      |OO__X
    """)

  val G = Game(B1)

}
class BoardTest extends FunSuite with Util {

  test("dfs should work on basic cases") {
    val ps = List[PlacedCell](Point(1, 1) -> b, Point(1, 2) -> b)
    val pss = PlacedCell(Point(2, 1), b) :: ps
    val psss: List[PlacedCell] = Point(2, 2) -> w :: pss
    val bbbb: List[PlacedCell] = Point(2, 2) -> b :: pss
    dfsTest(ps)
    dfsTest(pss)
    dfsTest(psss, Set(Point(2, 2) -> w))
    dfsTest(bbbb)
  }

  test("basic put works") {
    val ps = List(PlacedCell(Point(1, 1), b))
    val b1 = Board(2, GoMap(ps))
    assert(b1.at(1 -> 1) === BlackCell)
    assert(b1.at(1 -> 2) === EmptyCell)
    assert(b1.at(2 -> 1) === EmptyCell)
    assert(b1.at(2 -> 2) === EmptyCell)
  }

  test("capture works in bug case") {
    val ps: List[PlacedCell] = List(Point(2, 2) -> w, Point(2, 3) -> w, Point(3, 2) -> w, Point(3, 3) -> w, Point(1, 2) -> b, Point(2, 1) -> b)
    val b1 = Board(3, GoMap(ps))
    val b2 = b1.put(BlackCell, 3 -> 1)
    assert(b1.at(3 -> 1) == EmptyCell)
    assert(b2.at(3 -> 1) == BlackCell)
    assert(b2.at(3 -> 2) == WhiteCell)
  }

  test("fromString works") {
    val b1 = fromString(3,
      """
        |xxx
        |ooo
        |
      """.stripMargin)
    val expected: Set[PlacedCell] = Set((Point(1, 1), b), (Point(1, 2), b), (Point(1, 3), b), (Point(2, 1), w), (Point(2, 2), w), (Point(2, 3), w))
    val obtained = b1.cells.cells.toSet
    assert(obtained === expected)
    val b2 = fromString(3, "")
    assert(b2.cells.cells.isEmpty)
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

  test("Board.freeCells works") {
    val n = 5
    val B = fromString(n, "")

    val allCells = GoSet((for {
      x <- 1 to n
      y <- 1 to n
    } yield Point(x, y)).toList)

    assert(B.freeCells === allCells)
    assert(B.put(w, Point(1, 1)).freeCells === allCells - Point(1, 1))
  }


  test("Game.move OutsideOfErrorBoard") {
    assert(G.move(Place(0, 1)) === Right(OutsideOfBoardError))
    assert(G.move(Place(1, 0)) === Right(OutsideOfBoardError))
    assert(G.move(Place(5, 1)) === Right(OutsideOfBoardError))
    assert(G.move(Place(1, 5)) === Right(OutsideOfBoardError))
  }

  // Ignore because we have require statement
  test("Game.move AlreadyOccupiedError") {
    assert(G.move(Place(1, 2)) === Right(AlreadyOccupiedError))
    assert(G.move(Place(1, 3)) === Right(AlreadyOccupiedError))
    assert(G.move(Place(2, 1)) === Right(AlreadyOccupiedError))
    assert(G.move(Place(2, 2)) === Right(AlreadyOccupiedError))
    assert(G.move(Place(2, 4)) === Right(AlreadyOccupiedError))
    assert(G.move(Place(3, 2)) === Right(AlreadyOccupiedError))
    assert(G.move(Place(3, 3)) === Right(AlreadyOccupiedError))
  }

  test("Game.move SuicideError") {
    val g = Game(B1)
    val suicide = Place(1, 4)
    val ko1 = Place(2, 3)
    val ko2 = Place(2, 2)
    val g2 = Game(List(B2, B1), List(ko1))
    assert(g.move(suicide) === Right(SuicideError))
  }

  test("Game.move KoError") {
    val g = Game(B1)
    val suicide = Place(1, 4)
    val ko1 = Place(2, 3)
    val ko2 = Place(2, 2)
    val g2 = Game(List(B2, B1), List(ko1))
    assert(g2.move(ko2) === Right(KoError))
  }

}

class BoardTestPlayground extends FunSuite with Util {

}
