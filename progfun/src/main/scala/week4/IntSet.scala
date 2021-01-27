package week4

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet

}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
}

class NonEmpty(i: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < i) left contains x
    else if (x > i) right contains x
    else true

  def incl(x: Int): IntSet = {
    if (x < i) new NonEmpty(i, left incl x, right)
    else if (x > i) new NonEmpty(i, left, right incl x)
    else this
  }

  def union(other: IntSet): IntSet = ((left union right) union other) incl i
}
