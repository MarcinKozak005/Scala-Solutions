package P01_Elementary

import scala.io.StdIn.readLine

object P5Multiples5And3 {
  def P5Multiples5And3(): Unit = {
    print("Enter a number: ")
    var (sum, i, n) = (0, 0, readLine())
    for (i <- 1 to n.toInt) {
      if (i % 3 == 0 || i % 5 == 0) sum += i
    }
    println("Sum of x%3 and x%5 numbers 1 to " + n + " is: " + List.range(1, n.toInt).sum)
  }
}
