import scala.runtime.Nothing$

def removeAt[T](n: Int, xs: List[T]): List[T] = n match {
  case 0 => if (xs.isEmpty) throw new IndexOutOfBoundsException else xs.tail
  case _ =>  xs.head :: removeAt(n - 1, xs.tail)
}

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)

def flatten(xs: List[Any]): List[Any] = {
  def addToList(x: Any, result: List[Any]): List[Any] = x match {
    case List() => result
    case List(y) => addToList(y, result)
    case y :: ys => addToList(y, result) ::: flatten(ys)
    case z => z :: result
  }

  if (xs.isEmpty) List()
  else addToList(xs.head, List()) ::: flatten(xs.tail)
}





flatten(List(List(1, 1), 2, List(3, List(5, 8)))) //  List[Any] = List(1, 1, 2, 3, 5, 8)