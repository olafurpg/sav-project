package go.util

object Util {

  val debugLevel = 0


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
