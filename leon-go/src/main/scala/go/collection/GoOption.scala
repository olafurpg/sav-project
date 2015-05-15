package go.collection

sealed abstract class GoOption[T] {
  def isDefined: Boolean = {
    this match {
      case GoSome(e) => true
      case _ => false
    }
  }

  def orElse(or: GoOption[T]) = this match {
    case GoSome(v) => this
    case GoNone() => or
  }

  def getOrElse(default: T) = this match {
    case GoSome(v) => v
    case GoNone()  => default
  }

  // Higher-order API
  def map[R](f: T => R): GoOption[R] = { this match {
    case GoNone() => GoNone[R]()
    case GoSome(x) => GoSome(f(x))
  }} ensuring { _.isDefined == this.isDefined }

}
case class GoSome[T](e: T) extends GoOption[T]
case class GoNone[T]() extends GoOption[T]

