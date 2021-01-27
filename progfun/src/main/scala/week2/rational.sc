class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must not be zero")

  // This is an overloaded constructor
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val numer = x
  val denom = y

  def simpleNumer = numer / gcd(x, y)
  def simpleDenom = denom / gcd(x, y)

  def less(that: Rational) = numer * that.denom < that.numer * denom

  def unary_- : Rational = new Rational(-numer, denom)

  def + (that: Rational) = new Rational(numer* that.denom + that.numer * denom, denom * that.denom)

  def - (that: Rational) = this + -that

  override def toString() = simpleNumer + "/" + simpleDenom
}

