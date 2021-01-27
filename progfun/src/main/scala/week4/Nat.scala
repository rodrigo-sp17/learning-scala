package week4

// Peano numbers

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new NoSuchElementException
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = if (that.isZero) this else throw new NoSuchElementException
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor: Nat = n
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = new Succ(n + that)
  def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}
