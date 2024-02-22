class P19_Triangle {

}


class Triangle(a: Double, b: Double, c: Double) {
  val isTriangle: Boolean = (a + b >= c) && (b + c >= a) && (c + a >= b) && (a + b + c > 0)

  def equilateral: Boolean = isTriangle && (a == b) && (b == c)

  def isosceles: Boolean = isTriangle && ((a == b) || (b == c) || (a == c))

  def scalene: Boolean = isTriangle && (a != b) && (b != c) && (c != a)

}


object Triangle {
  def apply(a: Double, b: Double, c: Double) = new Triangle(a, b, c)
}


