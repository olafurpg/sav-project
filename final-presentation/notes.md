* require throws IllegalArgumentException while we use the type system instead to verify
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
