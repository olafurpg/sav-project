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


  implicit def intTuple2Point(tpl: (Int, Int)): Point = Point(tpl._1, tpl._2)

  implicit def bt2p(tpl: (BigInt, BigInt)): Point = bigIntTuple2Point(tpl)

  implicit def t2pc(tpl: (Point, Cell)): PlacedCell = tpl2PlacedCell(tpl)

  def fromString(N: Int, str: String): Board = {
    val cells = for {
      (row, x) <- str.stripMargin.split("\n").filter(!_.isEmpty).zipWithIndex
      (ch, y) <- {
        row.zipWithIndex
      }
    } yield PlacedCell(Point(x + 1, y + 1), cellFromString(ch))
    Board(N, GoMap(cells.filter(_.c != EmptyCell).toList))
  }

  def dfsTest(lst: List[PlacedCell], ignore: GoSet = GoSet.empty): Unit = {
    val expected = GoSet(GoMap(lst.filterNot(ignore.contains)))
    val b1 = Board(10, GoMap(lst))
    assert(CaptureLogic.connectedComponent(b1, PlacedCell(Point(1, 1), b)).isEqualTo(expected))
  }

  def moveTest(n: Int, str1: String, str2: String, c: Cell, p: Point): Unit = {
    val b1 = fromString(n, str1.stripMargin)
    val b2 = fromString(n, str2.stripMargin)
    assert(CaptureLogic.put(b1, p, c) === b2)
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

  val points = List(pc(1,1,b), pc(1, 2, b), pc(2, 1, b), pc(2, 2, b))
  def pc(x: Int, y: Int, c: Cell) = PlacedCell(Point(x, y), c)
  test("CaptureLogic.connectedComponent works") {
    val board = Board(3, GoMap(points))

    assert(CaptureLogic.connectedComponent(board, points.head).isEqualTo(GoSet(GoMap(points))))
  }

  test("Board constructor works") {
    val b1 = Board(3, GoMap(points))
    assert(b1.at(1 -> 1) === BlackCell)
    assert(b1.at(1 -> 2) === BlackCell)
    assert(b1.at(2 -> 1) === BlackCell)
    assert(b1.at(2 -> 2) === BlackCell)
    assert(b1.at(3 -> 1) === EmptyCell)
    assert(b1.at(3 -> 2) === EmptyCell)
    assert(b1.at(3 -> 3) === EmptyCell)
    assert(b1.at(1 -> 3) === EmptyCell)
    assert(b1.at(2 -> 3) === EmptyCell)
  }

  test("Board.put works") {
    val b1 = Board(3, GoMap(points)).put(w, Point(3, 3))

    assert(b1.at(1 -> 1) === BlackCell)
    assert(b1.at(1 -> 2) === BlackCell)
    assert(b1.at(2 -> 1) === BlackCell)
    assert(b1.at(2 -> 2) === BlackCell)
    assert(b1.at(3 -> 1) === EmptyCell)
    assert(b1.at(3 -> 2) === EmptyCell)
    assert(b1.at(3 -> 3) === WhiteCell)
    assert(b1.at(1 -> 3) === EmptyCell)
    assert(b1.at(2 -> 3) === EmptyCell)
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

    val allCells = (for {
      x <- 1 to n
      y <- 1 to n
    } yield PlacedCell(Point(x, y), EmptyCell)).toList

    assert(B.freeCells.m.cells === allCells)
    assert(B.put(w, Point(1, 1)).freeCells.m.cells === allCells.tail)
  }


  test("Game.move OutsideOfErrorBoard") {
    assert(Rule.check(G, Place(0, 1)) === Some(OutsideOfBoardError))
    assert(Rule.check(G, Place(1, 0)) === Some(OutsideOfBoardError))
    assert(Rule.check(G, Place(5, 1)) === Some(OutsideOfBoardError))
    assert(Rule.check(G, Place(1, 5)) === Some(OutsideOfBoardError))
  }

  // Ignore because we have require statement
  test("Game.move AlreadyOccupiedError") {
    assert(Rule.check(G, Place(1, 2)) === Some(AlreadyOccupiedError))
    assert(Rule.check(G, Place(1, 3)) === Some(AlreadyOccupiedError))
    assert(Rule.check(G, Place(2, 1)) === Some(AlreadyOccupiedError))
    assert(Rule.check(G, Place(2, 2)) === Some(AlreadyOccupiedError))
    assert(Rule.check(G, Place(2, 4)) === Some(AlreadyOccupiedError))
    assert(Rule.check(G, Place(3, 2)) === Some(AlreadyOccupiedError))
    assert(Rule.check(G, Place(3, 3)) === Some(AlreadyOccupiedError))
  }

  test("Game.move SuicideError") {
    val g = Game(B1)
    val suicide = Place(1, 4)
    val ko1 = Place(2, 3)
    val ko2 = Place(2, 2)
    val g2 = Game(List(B2, B1), List(ko1))
    assert(Rule.check(g, suicide) === Some(SuicideError))
  }

  test("Game.move KoError") {
    val g = Game(B1)
    val suicide = Place(1, 4)
    val ko1 = Place(2, 3)
    val ko2 = Place(2, 2)
    val g2 = Game(List(B2, B1), List(ko1))
    assert(Rule.check(g2, ko2) === Some(KoError))
  }

}

class BoardTestPlayground extends FunSuite with Util {

}
