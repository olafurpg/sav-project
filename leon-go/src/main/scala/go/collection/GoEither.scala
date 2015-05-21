package go.collection

sealed abstract class GoEither[A, B]

case class GoLeft[A, B](a: A) extends GoEither[A, B]
case class GoRight[A, B](b: B) extends GoEither[A, B]
