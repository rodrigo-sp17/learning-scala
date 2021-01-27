// Tail recursive function of factorial
def factorial(x: Int) : Int = {
  def factorialIter(total: Int, x: Int): Int =
    if (x == 0) total
    else factorialIter(total * x, x - 1)

  factorialIter(1, x)
}