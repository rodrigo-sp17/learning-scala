def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())((elem, y) => f(elem) :: y)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((elem, acc) => acc + 1)