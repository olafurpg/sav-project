object RuleEngine {
  def next(game: Game, step: Step): GoEither[Game, MoveError]
  def check(game: Game, step: Step): Option[MoveError]
  def isValid(game: Game, step: Step): Boolean
  def isOver(game: Game): Boolean
  def score(game: Game): Map[PlayerType, Int]
}
