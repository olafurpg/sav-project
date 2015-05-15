package go.collection

object ListOps {
  def flatten[T](ls: GoList[GoList[T]]): GoList[T] = ls match {
    case GoCons(h, t) => h ++ flatten(t)
    case GoNil() => GoNil()
  }
}
sealed abstract class GoList[T] {

  def size: BigInt = (this match {
    case GoNil() => BigInt(0)
    case GoCons(h, t) => 1 + t.size
  }) ensuring (_ >= 0)

  def content: Set[T] = this match {
    case GoNil() => Set()
    case GoCons(h, t) => Set(h) ++ t.content
  }

  def contains(v: T): Boolean = (this match {
    case GoCons(h, t) if h == v => true
    case GoCons(_, t) => t.contains(v)
    case GoNil() => false
  }) ensuring { _ == (content contains v) }

  def ++(that: GoList[T]): GoList[T] = (this match {
    case GoNil() => that
    case GoCons(x, xs) => GoCons(x, xs ++ that)
  }) ensuring { res =>
    (res.content == this.content ++ that.content) &&
      (res.size == this.size + that.size)
  }

  def head: T = {
    require(this != GoNil[T]())
    val GoCons(h, _) = this
    h
  }

  def tail: GoList[T] = {
    require(this != GoNil[T]())
    val GoCons(_, t) = this
    t
  }

  def apply(index: BigInt): T = {
    require(0 <= index && index < size)
    if (index == BigInt(0)) {
      head
    } else {
      tail(index-1)
    }
  }

  def ::(t:T): GoList[T] = GoCons(t, this)

  def :+(t:T): GoList[T] = {
    this match {
      case GoNil() => GoCons(t, this)
      case GoCons(x, xs) => GoCons(x, xs :+ (t))
    }
  } ensuring(res => (res.size == size + 1) && (res.content == content ++ Set(t)))

  def reverse: GoList[T] = {
    this match {
      case GoNil() => this
      case GoCons(x,xs) => xs.reverse :+ x
    }
  } ensuring (res => (res.size == size) && (res.content == content))

  def take(i: BigInt): GoList[T] = { (this, i) match {
    case (GoNil(), _) => GoNil[T]()
    case (GoCons(h, t), i) =>
      if (i <= BigInt(0)) {
        GoNil[T]()
      } else {
        GoCons(h, t.take(i-1))
      }
  }} ensuring { res =>
    res.content.subsetOf(this.content) && (res.size == (
      if      (i <= 0)         BigInt(0)
      else if (i >= this.size) this.size
      else                     i
      ))
  }

  def drop(i: BigInt): GoList[T] = { (this, i) match {
    case (GoNil(), _) => GoNil[T]()
    case (GoCons(h, t), i) =>
      if (i <= BigInt(0)) {
        GoCons[T](h, t)
      } else {
        t.drop(i-1)
      }
  }} ensuring { res =>
    res.content.subsetOf(this.content) && (res.size == (
      if      (i <= 0)         this.size
      else if (i >= this.size) BigInt(0)
      else                     this.size - i
      ))
  }

  def slice(from: BigInt, to: BigInt): GoList[T] = {
    require(0 <= from && from <= to && to <= size)
    drop(from).take(to-from)
  }

  def replace(from: T, to: T): GoList[T] = { this match {
    case GoNil() => GoNil[T]()
    case GoCons(h, t) =>
      val r = t.replace(from, to)
      if (h == from) {
        GoCons(to, r)
      } else {
        GoCons(h, r)
      }
  }} ensuring { (res: GoList[T]) =>
    res.size == this.size &&
      res.content == (
        (this.content -- Set(from)) ++
          (if (this.content contains from) Set(to) else Set[T]())
        )
  }

  private def chunk0(s: BigInt, l: GoList[T], acc: GoList[T], res: GoList[GoList[T]], s0: BigInt): GoList[GoList[T]] = l match {
    case GoNil() =>
      if (acc.size > 0) {
        res :+ acc
      } else {
        res
      }
    case GoCons(h, t) =>
      if (s0 == BigInt(0)) {
        chunk0(s, l, GoNil(), res :+ acc, s)
      } else {
        chunk0(s, t, acc :+ h, res, s0-1)
      }
  }

  def chunks(s: BigInt): GoList[GoList[T]] = {
    require(s > 0)

    chunk0(s, this, GoNil(), GoNil(), s)
  }

  def zip[B](that: GoList[B]): GoList[(T, B)] = { (this, that) match {
    case (GoCons(h1, t1), GoCons(h2, t2)) =>
      GoCons((h1, h2), t1.zip(t2))
    case _ =>
      GoNil[(T, B)]()
  }} ensuring { _.size == (
    if (this.size <= that.size) this.size else that.size
    )}

  def -(e: T): GoList[T] = { this match {
    case GoCons(h, t) =>
      if (e == h) {
        t - e
      } else {
        GoCons(h, t - e)
      }
    case GoNil() =>
      GoNil[T]()
  }} ensuring { res =>
    res.size <= this.size &&
      res.content == this.content -- Set(e)
  }

  def --(that: GoList[T]): GoList[T] = { this match {
    case GoCons(h, t) =>
      if (that.contains(h)) {
        t -- that
      } else {
        GoCons(h, t -- that)
      }
    case GoNil() =>
      GoNil[T]()
  }} ensuring { res =>
    res.size <= this.size &&
      res.content == this.content -- that.content
  }

  def &(that: GoList[T]): GoList[T] = { this match {
    case GoCons(h, t) =>
      if (that.contains(h)) {
        GoCons(h, t & that)
      } else {
        t & that
      }
    case GoNil() =>
      GoNil[T]()
  }} ensuring { res =>
    res.size <= this.size &&
      res.content == (this.content & that.content)
  }

  def padTo(s: BigInt, e: T): GoList[T] = { (this, s) match {
    case (_, s) if s <= 0 =>
      this
    case (GoNil(), s) =>
      GoCons(e, GoNil().padTo(s-1, e))
    case (GoCons(h, t), s) =>
      GoCons(h, t.padTo(s-1, e))
  }} ensuring { res =>
    if (s <= this.size)
      res == this
    else
      res.size == s &&
        res.content == this.content ++ Set(e)
  }

  def find(e: T): GoOption[BigInt] = { this match {
    case GoNil() => GoNone[BigInt]()
    case GoCons(h, t) =>
      if (h == e) {
        GoSome[BigInt](0)
      } else {
        t.find(e) match {
          case GoNone()  => GoNone[BigInt]()
          case GoSome(i) => GoSome(i+1)
        }
      }
  }} ensuring { _.isDefined == this.contains(e) }

  def init: GoList[T] = {
    require(!isEmpty)
    (this match {
      case GoCons(h, GoNil()) =>
        GoNil[T]()
      case GoCons(h, t) =>
        GoCons[T](h, t.init)
    })
  } ensuring ( (r: GoList[T]) =>
    r.size == this.size - 1 &&
      r.content.subsetOf(this.content)
    )

  def last: T = {
    require(!isEmpty)
    this match {
      case GoCons(h, GoNil()) => h
      case GoCons(_, t) => t.last
    }
  } ensuring { this.contains _ }

  def unique: GoList[T] = this match {
    case GoNil() => GoNil()
    case GoCons(h, t) =>
      GoCons(h, t.unique - h)
  }

  def splitAt(e: T): GoList[GoList[T]] =  split(GoCons(e, GoNil()))

  def split(seps: GoList[T]): GoList[GoList[T]] = this match {
    case GoCons(h, t) =>
      if (seps.contains(h)) {
        GoCons(GoNil(), t.split(seps))
      } else {
        val r = t.split(seps)
        GoCons(GoCons(h, r.head), r.tail)
      }
    case GoNil() =>
      GoCons(GoNil(), GoNil())
  }

  def evenSplit: (GoList[T], GoList[T]) = {
    val c = size/2
    (take(c), drop(c))
  }

  def insertAt(pos: BigInt, l: GoList[T]): GoList[T] = {
    if(pos < 0) {
      insertAt(size + pos, l)
    } else if(pos == BigInt(0)) {
      l ++ this
    } else {
      this match {
        case GoCons(h, t) =>
          GoCons(h, t.insertAt(pos-1, l))
        case GoNil() =>
          l
      }
    }
  } ensuring { res =>
    res.size == this.size + l.size &&
      res.content == this.content ++ l.content
  }

  def replaceAt(pos: BigInt, l: GoList[T]): GoList[T] = {
    if(pos < 0) {
      replaceAt(size + pos, l)
    } else if(pos == BigInt(0)) {
      l ++ this.drop(l.size)
    } else {
      this match {
        case GoCons(h, t) =>
          GoCons(h, t.replaceAt(pos-1, l))
        case GoNil() =>
          l
      }
    }
  } ensuring { res =>
    res.content.subsetOf(l.content ++ this.content)
  }

  def rotate(s: BigInt): GoList[T] = {
    if (s < 0) {
      rotate(size + s)
    } else if (s > size) {
      rotate(s - size)
    } else {
      drop(s) ++ take(s)
    }
  } ensuring { res =>
    res.size == this.size
  }

  def isEmpty = this match {
    case GoNil() => true
    case _ => false
  }

  // Higher-order API
  def map[R](f: T => R): GoList[R] = { this match {
    case GoNil() => GoNil[R]()
    case GoCons(h, t) => f(h) :: t.map(f)
  }} ensuring { _.size == this.size }

  def foldLeft[R](z: R)(f: (R,T) => R): R = this match {
    case GoNil() => z
    case GoCons(h,t) => t.foldLeft(f(z,h))(f)
  }

  def foldRight[R](z: R)(f: (T,R) => R): R = this match {
    case GoNil() => z
    case GoCons(h, t) => f(h, t.foldRight(z)(f))
  }

  def scanLeft[R](z: R)(f: (R,T) => R): GoList[R] = { this match {
    case GoNil() => z :: GoNil()
    case GoCons(h,t) => z :: t.scanLeft(f(z,h))(f)
  }} ensuring { !_.isEmpty }

  def scanRight[R](z: R)(f: (T,R) => R): GoList[R] = { this match {
    case GoNil() => z :: GoNil[R]()
    case GoCons(h, t) =>
      val rest@GoCons(h1,_) = t.scanRight(z)(f)
      f(h, h1) :: rest
  }} ensuring { !_.isEmpty }

  def flatMap[R](f: T => GoList[R]): GoList[R] =
    ListOps.flatten(this map f)

  def filter(p: T => Boolean): GoList[T] = { this match {
    case GoNil() => GoNil[T]()
    case GoCons(h, t) if p(h) => GoCons(h, t.filter(p))
    case GoCons(_, t) => t.filter(p)
  }} ensuring { res =>
    res.size <= this.size &&
      res.content.subsetOf(this.content) &&
      res.forall(p)
  }

  def filterNot(p: T => Boolean): GoList[T] =
    filter(!p(_)) ensuring { res =>
      res.size <= this.size &&
        res.content.subsetOf(this.content) &&
        res.forall(!p(_))
    }

  def partition(p: T => Boolean): (GoList[T], GoList[T]) = { this match {
    case GoNil() => (GoNil[T](), GoNil[T]())
    case GoCons(h, t) =>
      val (l1, l2) = t.partition(p)
      if (p(h)) (h :: l1, l2)
      else      (l1, h :: l2)
  }} ensuring { res =>
    res._1 == filter(p) &&
      res._2 == filterNot(p)
  }

  // In case we implement for-comprehensions
  def withFilter(p: T => Boolean) = filter(p)

  def forall(p: T => Boolean): Boolean = this match {
    case GoNil() => true
    case GoCons(h, t) => p(h) && t.forall(p)
  }

  def exists(p: T => Boolean) = !forall(!p(_))

  def find(p: T => Boolean): GoOption[T] = { this match {
    case GoNil() => GoNone[T]()
    case GoCons(h, t) if p(h) => GoSome(h)
    case GoCons(_, t) => t.find(p)
  }} ensuring { _.isDefined == exists(p) }

  def groupBy[R](f: T => R): Map[R, GoList[T]] = this match {
    case GoNil() => Map.empty[R, GoList[T]]
    case GoCons(h, t) =>
      val key: R = f(h)
      val rest: Map[R, GoList[T]] = t.groupBy(f)
      val prev: GoList[T] = if (rest isDefinedAt key) rest(key) else GoNil[T]()
      (rest ++ Map((key, h :: prev))) : Map[R, GoList[T]]
  }

  def takeWhile(p: T => Boolean): GoList[T] = { this match {
    case GoCons(h,t) if p(h) => GoCons(h, t.takeWhile(p))
    case _ => GoNil[T]()
  }} ensuring { res =>
    (res forall p) &&
      (res.size <= this.size) &&
      (res.content subsetOf this.content)
  }

  def dropWhile(p: T => Boolean): GoList[T] = { this match {
    case GoCons(h,t) if p(h) => t.dropWhile(p)
    case _ => this
  }} ensuring { res =>
    (res.size <= this.size) &&
      (res.content subsetOf this.content) &&
      (res.isEmpty || !p(res.head))
  }

  def count(p: T => Boolean): BigInt = { this match {
    case GoNil() => BigInt(0)
    case GoCons(h, t) =>
      (if (p(h)) BigInt(1) else BigInt(0)) + t.count(p)
  }} ensuring {
    _ == this.filter(p).size
  }

}


case class GoCons[T](h: T, t: GoList[T]) extends GoList[T]
case class GoNil[T]() extends GoList[T]
