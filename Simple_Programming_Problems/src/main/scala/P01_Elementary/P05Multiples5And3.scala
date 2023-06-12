package P01_Elementary

import scala.io.StdIn.readLine

object P05Multiples5And3 extends App {
  print("Enter a number: ")
  val n = readLine()

  def solution1(): Unit = {
    var sum = 0
    for (i <- 1 to n.toInt) {
      if (i % 3 == 0 || i % 5 == 0) sum += i
    }
    println(s"Sum of x%3 and x%5 numbers 1 to $n is: $sum")
  }

  def solution2(): Unit = {
    println(s"Sum of x%3 and x%5 numbers 1 to $n is: ${
      (for {
        i <- 1 to n.toInt
        if i % 3 == 0 || i % 5 == 0
      } yield i).sum
    }")
  }
  solution1()
  solution2()
}
