package leon

package object collection {
  type List[T] = scala.collection.immutable.List[T]
  implicit class Content[T](lst: List[T]) {
    def content: Set[T] = lst.toSet
  }
}
