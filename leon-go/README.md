# Board game Go in Leon and Scala

This project is an implementation of Go in Leon and Scala with the objective to see how practical it is to use Leon to program a correct implementation of Go.

## Run

Execute following commmand and enjoy:-)

``` shell
sbt run
```

## Test

To run verification and unit tests, execute following command under project root directory:

``` shell
sbt verify
```

## Design

### Game

Leon doesn't support side effects, so we modeled the game as a
finite list of board states as follows:

``` Scala
case class Board(n: BigInt, cells: GoMap[Point, Cell])

case class Game(states: List[Board], steps: List[Step], activePlayer: PlayerType, size: BigInt)
```

The type `Cell` represents the color of the stone, it's defined as
follows:

``` Scala
abstract class Cell
case object WhiteCell extends Cell
case object BlackCell extends Cell
case object EmptyCell extends Cell
```

The type `Step` represents an action that can be taken by the player, it's
defined as follows:

``` Scala
sealed abstract class Step
case object Pass extends Step
case class Place(x: BigInt, y: BigInt) extends Step
```

The type `PlayerType` represents the two sides of a game, it's defined as
follows:

``` Scala
sealed abstract class PlayerType
case object WhitePlayer extends PlayerType
case object BlackPlayer extends PlayerType
```

### Rules

Each game has rules that should be observed by the players. Inside the world of
Go, there're several popular rules, such as Chinese rule, Japanese rule, etc.
In order to decouple rules from the game, we implemented the rules in
`RuleEngine`, so that it's possible to implement different sets of rules.
The object `RuleEngine` is defined as follows:

``` Scala
object RuleEngine {
  def next(game: Game, step: Step): GoEither[Game, MoveError]
  def check(game: Game, step: Step): Option[MoveError]
  def isValid(game: Game, step: Step): Boolean
  def isOver(game: Game): Boolean
  def score(game: Game): Map[PlayerType, Int]
}
```

There could be several types of move errors, which are defined as follows:

``` Scala
abstract class MoveError

case object KoError extends MoveError
case object OutsideOfBoardError extends MoveError
case object AlreadyOccupiedError extends MoveError
case object SuicideError extends MoveError
```

### Player

There are several types of players in the game, such as computer players, human
players, etc. We impose that all kinds of player should implement following
interface:

``` Scala
trait Player {
  def move(g: Game): Step
  def name: String
}
```

When the method `move` is called, the player should decide an action based
on the given state of the game. Currently we implemented three types of players
in the game:

- HumanPlayer: make a move from user input in the console
- ComputerPlayer: make a move based on MinMax algorithm
- RandomPlayer: make a random move

### Driver

The game is driven by the driver. An example driver is as follows:

``` Scala
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
```

With the driver defined as above, it's easy to run a game like following:

``` Scala
run(Game(5), Map(BlackPlayer -> HumanPlayer,  WhitePlayer -> RandomPlayer),
    stepCallback, errorCallback, resultCallback)
```
