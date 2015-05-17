package leon

/** Defines leon annotations
  *
  * Reference: http://leon.epfl.ch/doc/library.html
  */
package object annotation {
  case class ignore() extends scala.annotation.StaticAnnotation
  case class library() extends scala.annotation.StaticAnnotation
  case class induct() extends scala.annotation.StaticAnnotation
}
