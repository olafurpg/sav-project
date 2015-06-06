def positive(i: Int): Int = {
  require(i > 0)
  i
}

List(1, 2, 3).foldLeft(0) { case (_, i) =>
  positive(i) // precondition fails
}

def test(list: List[BigInt]): List[BigInt] = {
  list.filter(x=> x > 1 && x < 100)
} ensuring { res =>
  res.forall(x => x > 1) && res.forall(x => x < 100)
  // => FAILS
  res.forall(x=> x > 1 && x < 100)
  // => HOLDS
  res.forall(x=> x > 1)
  // => FAILS
}
