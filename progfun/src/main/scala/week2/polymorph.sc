// Function nth
// Takes integer n and a list
// Selects nth element of list
// Elements numbered from 0
// Throw IOUBE if index outside range (from 0 up to the list length - 1)
def nth[T](n: Int, list: List[T]): T = {
  def nthIter[T](index: Int, n: Int, xs: List[T]): T =
    if (index == n) xs.head
    else nthIter(index + 1, n, xs.tail)

  if (n > list.size - 1) throw new IndexOutOfBoundsException
  else nthIter(0, n, list)
}
