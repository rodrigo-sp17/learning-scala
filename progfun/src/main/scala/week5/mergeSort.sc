def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x :: xs1, y :: ys1) => {
      if (x > y) y :: merge(xs, ys1)
      else x :: merge(xs1, ys)
    }
  }