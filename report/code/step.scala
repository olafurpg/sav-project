sealed abstract class Step
case object Pass extends Step
case class Place(x: BigInt, y: BigInt) extends Step
