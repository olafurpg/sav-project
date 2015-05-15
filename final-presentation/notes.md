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

* sealed traits can have similar semantics to sealed abstract class
* No pattern matching in ensuring?
[ Error  ] 16:12: error: pattern type is incompatible with expected type;
[ Error  ]  found   : go.core.Pass.type
[ Error  ]  required: go.core.Step.type
                 case Pass => res == true
                      ^
