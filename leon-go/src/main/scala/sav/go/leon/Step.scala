package sav.go.leon

sealed trait Step
case object Pass extends Step
case class Place(x: Int, y: Int) extends Step

