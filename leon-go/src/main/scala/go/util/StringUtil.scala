package go.util

import leon.lang.string
import go.core.CellObject._

trait StringUtil {

  val debugLevel = 0

  def cellFromString(ch: Char): Cell = ch match {
    case 'X' | 'x' => BlackCell
    case 'O' | 'o' => WhiteCell
    case _ => EmptyCell
  }

  def cellToString(c: Cell): String = c match {
    case WhiteCell => "O"
    case BlackCell => "O"
    case EmptyCell => "_"
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
