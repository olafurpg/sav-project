package go.core

import leon.annotation._
import go.core.definitions._

@ignore
trait Player {
  def move(g: Game): Step
  def name: String
}
