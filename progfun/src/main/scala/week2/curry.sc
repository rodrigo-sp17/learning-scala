import scala.math.abs

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 0
  else f(a) + sum(f)(a + 1, b)
}

sum(x => x)(0, 3)

def product(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) 1
  else f(a) * product(f)(a + 1, b)
}

product(x => x * x)(3, 7)

def factorial(n: Int) = product(x => x)(1, n)
factorial(4)

def apply(f: Int => Int)(g:(Int, Int) => Int)(a: Int, b: Int, c: Int): Int = {
  if (a > b) c
  else g(f(a), apply(f)(g)(a + 1, b, c))
}

apply(x => x*x)((d,e) => d * e)(3, 7, 1)


