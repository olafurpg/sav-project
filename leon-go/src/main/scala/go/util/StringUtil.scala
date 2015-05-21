package go.util

import go.core.Board
import leon.lang.string
import go.core.definitions._

trait StringUtil {

  val debugLevel = 0

  def cellFromString(ch: Char): Cell = ch match {
    case 'X' | 'x' => BlackCell
    case 'O' | 'o' => WhiteCell
    case _ => EmptyCell
  }

  def cellToString(c: Cell): String = c match {
    case WhiteCell => "O"
    case BlackCell => "X"
    case EmptyCell => "_"
  }

  def boardToString(b: Board) = {
    b.board.foldRight("\n") { (line, acc) => line.map(cellToString).mkString(" ") + "\n" + acc }
  }

  def log[T](msg: => T, level: Int = 0): Unit = if (level > debugLevel) {
    println(msg)
  }

  def lineDivider(n: Int): String = "*" * n

  def message(msg: String*): String = {
    val msgs = msg.toList
    val line = lineDivider(msgs.maxBy(_.length).length + 2)
    ((line :: msgs.map("* " + _)) :+ line).mkString("\n")
  }

}
