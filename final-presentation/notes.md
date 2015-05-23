* Missing useful collections (Map, Set)
* Missing methods on collections (filterNot)
* Sometimes cryptic error messages when porting Scala to Leon, esp. since
  filenames are missing from compiler errors:
    Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Any
[ Error  ] Unknown class scala.List
[ Error  ] Unknown class scala.List
[ Error  ] Unknown class scala.List
[ Error  ] Unknown class scala.List
[ Error  ] Unknown class scala.List
[ Error  ] Unknown class scala.List
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Either
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Predef.String
[ Error  ] Unknown class scala.Option

[ Error  ] 67:3: Function neighboors could not be extracted. The function likely uses features not supported by Leon.
             def neighboors(x: Int, y: Int): List[PlacedCell] =

[ Error  ] 28:63: Could not extract as PureScala (Scala tree of type class scala.reflect.internal.Trees$Ident)
             def at(x: Int, y: Int): Cell = cells.getOrElse(Point(x, y), EmptyCell)

Compiler even gives runtimeerror

java.lang.RuntimeException: Constructing match expression on non-supported type: <untyped>
	at scala.sys.package$.error(package.scala:27)

* sealed traits can have similar semantics to sealed abstract class
* No pattern matching in ensuring?
[ Error  ] 16:12: error: pattern type is incompatible with expected type;
[ Error  ]  found   : go.core.Pass.type
[ Error  ]  required: go.core.Step.type
                 case Pass => res == true
                      ^

Why do classes need to be inside of an object???
  
This gives error
31:Don't know what to do with this. Not purescala?
object Foo {
}
sealed abstract class Cell

case object WhiteCell extends Cell
case object BlackCell extends Cell
case object EmptyCell extends Cell

while this is OK
object Foo {
    sealed abstract class Cell
    case object WhiteCell extends Cell
    case object BlackCell extends Cell
    case object EmptyCell extends Cell
}

Great job Etienne!
9c7e02b5 src/main/scala/leon/frontends/scalac/CodeExtraction.scala (Etienne Kneuss              2014-02-06 13:38:09 +0100  870)           outOfSubsetError(tree, "Don't know what to do with this. Not purescala?");


We wanted to have a modular code base


Why can't we use strings? TODO: Stackoverflow

Why do type parameters make the verification so much more complicated?

Keep your collections simple.

Nil and Option are singletons in Scala while they're case classes in Leon.

Functions parameter to leon is useful, but it's annoying to type "Object\$method"

Signature of List.count is not the same as in scala.collection, takes higher order function

Leon was unable to detect bugs that our basic unit tests caught:

  def connectedComponent(board: Board, p: PlacedCell, visited: GoSet[PlacedCell] = GoSet.empty): GoSet[PlacedCell] = {
    require(board.isValid &&
      board.insideBoard(p) &&
      visited.isValid &&
      visited.forall(board.insideBoard)
    )

    if (visited.contains(p)) visited
    else {
      val newVisited = visited + p
      val toVisit = board.sameColorNeighbors(p)
      toVisit.foldLeft(newVisited) { (b, a) =>
        connectedComponent(board, a, b)
      }
    }
  } ensuring(_.isValid)

  def capturedCells(board: Board): GoSet[PlacedCell] = {
    require(board.isValid)

    val e = GoSet.empty[PlacedCell]
    board.cells.foldRight(e -> e) { case (p, (explored, captured)) =>
      if (explored.contains(PlacedCell(p._1, p._2))) explored -> captured
      else {
        val component = connectedComponent(board, PlacedCell(p._1, p._2))
        if (component.exists(hasLiberty(board))) (explored ++ component, captured)
        else (explored ++ component, captured ++ component)
      }
    }._2
  } ensuring { res =>
    res.isValid && board.cells.pairs.map(tpl2PlacedCell).forall { x =>
      implies(hasLiberty(board)(x), !res.contains(x)) &&
      implies(!hasLiberty(board)(x), res.contains(x))
      // Missing condition when x has no liberty but neighbor has liberty
    }
  }


We even tried to limit the size of the board to 3 and Leon couldn't find a counterexample in 20s.

Leon parsing is buggy, in this case we imported scala.collection.List:

Unknown class scala.List
java.lang.IndexOutOfBoundsException: 2
    at scala.collection.LinearSeqOptimized$class.apply(LinearSeqOptimized.scala:65)
    at scala.collection.immutable.List.apply(List.scala:84)
    at leon.frontends.scalac.CodeExtraction$Extraction.leon$frontends$scalac$CodeExtraction$Extraction$$registerDefaultMethod(CodeExtraction.scala:483)
    at leon.frontends.scalac.CodeExtraction$Extraction$$anonfun$leon$frontends$scalac$CodeExtraction$Extraction$$collectFunSigs$1.apply(CodeExtraction.scala:699)
    at leon.frontends.scalac.CodeExtraction$Extraction$$anonfun$leon$frontends$scalac$CodeExtraction$Extraction$$collectFunSigs$1.apply(CodeExtraction.scala:685)
    at scala.collection.immutable.List.foreach(List.scala:381)

Use headOption
8af3310b src/main/scala/leon/frontends/scalac/CodeExtraction.scala (Emmanouil (Manos) Koukoutos 2014-04-11 17:44:14 +0200  483)       val theParam = paramOwner.paramss.head(index)

Apparently, Leon doesn't support method overloading

These seem identical, but using the stdlib alone doesn't work

//      val newComponent = addElement(board, component, p)
//      val newToVisit = addElements(board, toVisit, board.sameColorNeighbors(p))
      val newComponent = p :: component
      val newToVisit = board.sameColorNeighbors(p) ++ toVisit

It would be nice to know what conditions inside a require or ensuring block are the hard ones to prove.
