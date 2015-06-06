def run(game: Game, players: Map[PlayerType, Player],
        stepCallback: (Game, PlayerType, Step) => Unit,
        errorCallback: (Game, MoveError) => Unit,
        resultCallback: (Game, Map[PlayerType, Int]) => Unit): Unit = {

  if (!RuleEngine.isOver(game)) {
    val player = players(game.activePlayer)
    val step = player.move(game)
    RuleEngine.next(game, step) match {
      case GoLeft(newGame) =>
        stepCallback(newGame, game.activePlayer, step)
        run(newGame, players, stepCallback, errorCallback, resultCallback)
      case GoRight(err) =>
        errorCallback(game, err)
        run(game, players, stepCallback, errorCallback, resultCallback)
    }
  }
  else {
    val score = RuleEngine.score(game)
    resultCallback(game, score)
  }
}

