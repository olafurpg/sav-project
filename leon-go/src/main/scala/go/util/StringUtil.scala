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
    mkString(b.board.map(mkString(_, "")), "\n", "\n", "\n")
  }

  def mkString[T](lst: List[T], sep: String): String = mkString(lst, "", sep, "")

  def mkString[T](lst: List[T], start: String, sep: String, end: String): String = lst match {
    case l if l.size == 0 => ""
    case l if l.size == 1 => start + l.head.toString + end
    case l if l.size > 1 => start + l.head.toString + sep + mkString(l.tail, "", sep, "") + end
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
