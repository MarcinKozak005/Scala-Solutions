import scala.math.pow

class P52_DifferenceOfSquares {

}


object DifferenceOfSquares {

  def sumOfSquares(n: Int): Int = (1 to n).map(v => v * v).sum

  def squareOfSum(n: Int): Int = pow((1 to n).sum, 2).toInt

  def differenceOfSquares(n: Int): Int = squareOfSum(n) - sumOfSquares(n)
}
