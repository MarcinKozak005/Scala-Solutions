import scala.math._
import scala.io.StdIn.readLine

object Solution {

  // My code
  def gcd(x: Int, y: Int): Int = {
    if (x * y == 0) max(x, y)
    else {
      val g = max(x, y)
      val l = min(x, y)
      gcd(g % l, l)
    }
  }
  // End of my code

  def acceptInputAndComputeGCD(pair: List[Int]): Unit =
    println(gcd(pair.head, pair.reverse.head))

  def main(args: Array[String]): Unit =
    acceptInputAndComputeGCD(
      readLine().trim().split(" ").map(x => x.toInt).toList
    )
}
