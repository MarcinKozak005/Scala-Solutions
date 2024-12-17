import scala.io.StdIn.readInt
import scala.annotation.tailrec

object Solution {

  @tailrec
  def fibonacciHelper(limit: Int, current: Int, p1: Int, p2: Int): Int =
    limit match {
      case 1                 => 0
      case 2 | 3             => 1
      case n if n == current => p2
      case _                 => fibonacciHelper(limit, current + 1, p2, p1 + p2)
    }

  def fibonacci(x: Int): Int = {
    fibonacciHelper(x, 2, 0, 1)
  }

  def main(args: Array[String]): Unit = {
    println(fibonacci(readInt()))
  }
}
